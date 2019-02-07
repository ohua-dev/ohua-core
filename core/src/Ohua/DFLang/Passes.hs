-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- Passes required to transform an expression in ALang into an expression in DFLang.
--
module Ohua.DFLang.Passes where

import Ohua.Prelude

import Control.Lens (at, non)
import Control.Monad (msum)
import Control.Monad.Tardis
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Ohua.ALang.Lang
import Ohua.ALang.PPrint

import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import Ohua.Stage

type Pass m
     = QualifiedBinding -> FnId -> Binding -> [Expression] -> m (Seq LetExpr)

runCorePasses :: (MonadOhua m, Pretty DFExpr) => DFExpr -> m DFExpr
runCorePasses expr = do
    let ctrlOptimized = collapseNth (== nodeRef Refs.ctrl) expr
    stage "ctrl-optimization" ctrlOptimized
    let ifOptimized = collapseNth (== nodeRef Refs.ifFun) ctrlOptimized
    stage "if-optimization" ifOptimized
    let smapOptimized = collapseNth (== nodeRef Refs.smapFun) ifOptimized
    stage "smap-optimized" smapOptimized
    return smapOptimized

-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Container c, Element c ~ LetExpr, MonadOhua m) => c -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = output le
            f a
                | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b -> failWith $ "Rebinding of " <> show b
            Nothing -> return ()
        modify (addAll produced)
    addAll = flip $ foldr' HS.insert

-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: MonadOhua m => DFExpr -> m ()
checkSSAExpr (DFExpr l _) = checkSSA l

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: MonadOhua m => Expression -> m DFExpr
lowerALang expr
    -- traceM $ "Lowering alang expr: " <> quickRender expr
 = do
    logDebugN $ "Lowering alang expr: " <> quickRender expr
    (var, exprs) <- runWriterT $ lowerToDF expr
    return $ DFExpr exprs var

--    (var, exprs) <- runWriterT (go expr)
lowerToDF ::
       (MonadOhua m, MonadWriter (Seq LetExpr) m) => Expression -> m Binding
lowerToDF (Var bnd) = pure bnd
lowerToDF (Let assign expr rest) = do
    logDebugN "Lowering Let -->"
    handleDefinitionalExpr assign expr continuation
  where
    continuation = lowerToDF rest
lowerToDF g = failWith $ "Expected `let` or binding: " <> show g

handleDefinitionalExpr ::
       (MonadOhua m, MonadWriter (Seq LetExpr) m)
    => Binding
    -> Expression
    -> m Binding
    -> m Binding
handleDefinitionalExpr assign l@(Apply _ _) cont = do
    (fn, fnId, s, args) <- handleApplyExpr l
    e <- lowerDefault fn fnId assign args
    tell $ pure e {stateArgument = s}
    cont
handleDefinitionalExpr _ e _ =
    failWith $ "Definitional expressions in a let can only be 'apply' but got: " <>
    show e

-- | Lower any not specially treated function type.
lowerDefault ::
       MonadOhua m
    => QualifiedBinding
    -> FnId
    -> Binding
    -> [Expression]
    -> m LetExpr
lowerDefault fn fnId assign args =
    mapM expectVar args <&> \args' ->
        LetExpr fnId [assign] (lowerFnToDFLang fn) Nothing args'
  where
    lowerFnToDFLang = fromMaybe (EmbedSf fn) . Refs.lowerBuiltinFunctions

-- | Analyze an apply expression, extracting the inner stateful
-- function and the nested arguments as a list.  Also generates a new
-- function id for the inner function should it not have one yet.
handleApplyExpr ::
       (MonadOhua m)
    => Expression
    -> m (QualifiedBinding, FnId, Maybe DFVar, [Expression])
handleApplyExpr l@(Apply _ _) = go [] l
  where
    go args =
        \case
            ve@Var {} ->
                fromEnv (options . callLocalFunction) >>= \case
                    Nothing ->
                        failWith
                            "Calling local functions is not supported in this adapter"
                    Just fn -> (fn, , Nothing, ve : args) <$> generateId
            PureFunction fn fnId ->
                (fn, , Nothing, args) <$> maybe generateId return fnId
            StatefulFunction fn fnId state -> do
                state' <- expectVar state
                (fn, , Just $ state', args) <$> maybe generateId return fnId
            ve@(Lit v) ->
                case v of
                    EnvRefLit _ ->
                        fromEnv (options . callEnvExpr) >>= \case
                            Nothing ->
                                failWith
                                    "Calling environment functions is not supported in this adapter"
                            Just fn -> (fn, , Nothing, ve : args) <$> generateId
                    other ->
                        throwError $
                        "This literal cannot be used as a function :" <>
                        show (pretty other)
            Apply fn arg -> go (arg : args) fn
            x ->
                failWith $ "Expected Apply or Var but got: " <>
                show (x :: Expression)
handleApplyExpr (PureFunction fn fnId) =
    (fn, , Nothing, []) <$> maybe generateId return fnId
                                                                                 -- what is this?
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

-- | Inspect an expression expecting something which can be captured
-- in a DFVar otherwise throws appropriate errors.
expectVar :: MonadError Error m => Expression -> m DFVar
expectVar (Var bnd) = pure $ DFVar bnd
-- TODO currently only allowed for the unitFn function
-- expectVar r@PureFunction {} =
--     throwError $
--     "Function references are not yet supported as arguments: " <>
--     show (pretty r)
expectVar (Lit l) = pure $ DFEnvVar l
expectVar a =
    failWith $ "Argument must be local binding or literal, was " <> show a

-- In this function I use the so called 'Tardis' monad, which is a special state
-- monad. It has one state that travels "forward" in time, which is the same as
-- the regular state monad, bu it also has a second state that uses lazyness to
-- travel "backwards" in time, meaning that reading the state gives you the
-- value you'll be setting later. This works fine so long as there are no cyclic
-- dependencies between the states (which is fairly easy to get wrong).
--
-- Anyhow the way its used here is that when I find a target function (specified
-- by the `selectionFunction`), I records its outputs in the forwards traveling
-- state to signal that functions using it should be removed. I then look at the
-- backwards traveling state to see which bindings the destructuring of this
-- function created and I use them to compose its new output.
--
-- When I find an `nth` I look up its inputs in the forward traveling state. If
-- I find an entry, then this `nth` belongs to a destructuring that should be
-- collapsed. I remove the nth and I record the output binding and the index the
-- nth got in the *backwards traveling state*.
--
-- In this way I can use the backwards traveling state to look into the future
-- and immediately see the bindings that a function destructures into. This is
-- what allows me to write this transformation with just a single DFLang pass.
collapseNth :: (QualifiedBinding -> Bool) -> DFExpr -> DFExpr
collapseNth selectionFunction =
    field @"letExprs" %~ Seq.fromList . catMaybes .
    flip evalTardis (mempty, mempty) .
    traverse go .
    toList
  where
    go e@LetExpr {output = [oldOut], functionRef = DFFnRef _ fun}
        | selectionFunction fun = do
            removedVals <- requestRemoval oldOut
              -- TODO do error handling here. Make sure no index is missing
            let newOuts = IM.elems removedVals
            return $
                Just
                    e {output = newOuts, functionRef = DFFnRef OperatorNode fun}
        | [DFEnvVar (NumericLit index), _len, DFVar source] <- callArguments e = do
            toRemove <- getPast
            ifM
                (queryRemoval source)
                (recordRemoval source oldOut index >> return Nothing)
                (return $ Just e)
    go e = return $ Just e
    requestRemoval bnd
        -- Record the binding as source for removal
     = do
        modifyForwards $ HS.insert bnd
        -- Ask the future which bindings it was destructured into
        getsFuture $ view $ at bnd . non mempty
    queryRemoval = getsPast . HS.member
    -- Tell the past that this binding was destructured at this index
    recordRemoval ::
           MonadTardis (HM.HashMap Binding (IM.IntMap Binding)) any m
        => Binding
        -> Binding
        -> Integer
        -> m ()
    recordRemoval source produced (fromInteger -> index) =
        modifyBackwards $ at source . non mempty . at index .~ Just produced
