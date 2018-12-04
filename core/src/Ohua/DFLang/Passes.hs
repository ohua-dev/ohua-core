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

import Control.Monad (msum)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Sequence (Seq)

import Ohua.ALang.Lang
import Ohua.ALang.PPrint

import Ohua.DFLang.Lang (DFExpr(..), DFFnRef(..), DFVar(..), LetExpr(..))
import Ohua.DFLang.Passes.Control
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import Ohua.Stage

type Pass m
     = QualifiedBinding -> FnId -> Binding -> [Expression] -> m (Seq LetExpr)

runCorePasses :: (MonadOhua m, Pretty DFExpr) => DFExpr -> m DFExpr
runCorePasses expr =
    let ctrlOptimized = optimizeCtrl expr
     in do stage "ctrl-optimization" ctrlOptimized
           return ctrlOptimized

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
    (fn, fnId, args) <- handleApplyExpr l
    tell =<< lowerDefault fn fnId assign args
    cont
handleDefinitionalExpr _ e _ =
    failWith $
    "Definitional expressions in a let can only be 'apply' but got: " <> show e

-- | Lower any not specially treated function type.
lowerDefault :: MonadOhua m => Pass m
lowerDefault fn fnId assign args =
    mapM expectVar args <&> \args' ->
        [LetExpr fnId [assign] (lowerFnToDFLang fn) args' Nothing]
  where
    lowerFnToDFLang = fromMaybe (EmbedSf fn) . Refs.lowerBuiltinFunctions

-- | Analyze an apply expression, extracting the inner stateful
-- function and the nested arguments as a list.  Also generates a new
-- function id for the inner function should it not have one yet.
handleApplyExpr ::
       (MonadOhua m) => Expression -> m (QualifiedBinding, FnId, [Expression])
handleApplyExpr l@(Apply _ _) = go [] l
  where
    go args =
        \case
            ve@Var {} ->
                fromEnv (options . callLocalFunction) >>= \case
                    Nothing ->
                        failWith
                            "Calling local functions is not supported in this adapter"
                    Just fn -> (fn, , ve : args) <$> generateId
            Sf fn fnId -> (fn, , args) <$> maybe generateId return fnId
            ve@(Lit v) ->
                case v of
                    EnvRefLit _ ->
                        fromEnv (options . callEnvExpr) >>= \case
                            Nothing ->
                                failWith
                                    "Calling environment functions is not supported in this adapter"
                            Just fn -> (fn, , ve : args) <$> generateId
                    other ->
                        throwError $
                        "This literal cannot be used as a function :" <>
                        show (pretty other)
            Apply fn arg -> go (arg : args) fn
            x ->
                failWith $
                "Expected Apply or Var but got: " <> show (x :: Expression)
handleApplyExpr (Sf fn fnId) = (fn, , []) <$> maybe generateId return fnId
                                                                                 -- what is this?
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

tieContext0 ::
       ( Monad m
       , Functor f
       , SemigroupConstraint (f LetExpr)
       , f LetExpr ~ c
       , Container c
       , Element c ~ LetExpr
       )
    => m (Maybe (c, Binding))
    -> c
    -> m c
tieContext0 initExpr lets
    | all hasLocalArcs lets = pure lets
    | otherwise =
        initExpr <&>
        maybe
            lets
            (\(initExprs, scopeBnd) -> initExprs <> fmap (go scopeBnd) lets)
  where
    hasLocalArcs e
        | any isLocalArc (callArguments e) = True
        | Just ctxArg <- contextArg e
        , isLocalArc (DFVar ctxArg) = True
    hasLocalArcs _ = False
    go _ e
        | hasLocalArcs e = e
    go ctxSource e = e {contextArg = Just ctxSource}
    isLocalArc (DFVar _) = True
    isLocalArc _ = False

-- | Inspect an expression expecting something which can be captured
-- in a DFVar otherwise throws appropriate errors.
expectVar :: MonadError Error m => Expression -> m DFVar
expectVar (Var bnd) = pure $ DFVar bnd
expectVar r@Sf {} =
    throwError $
    "Stateful function references are not yet supported as arguments: " <>
    show (pretty r)
expectVar (Lit l) = pure $ DFEnvVar l
expectVar a =
    failWith $ "Argument must be local binding or literal, was " <> show a
