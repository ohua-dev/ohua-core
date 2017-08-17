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
{-# LANGUAGE CPP             #-}
{-# LANGUAGE OverloadedLists #-}
module Ohua.DFLang.Passes where


import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Data.Sequence        (Seq, (<|), (><), (|>))
import qualified Data.Sequence        as S
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang     (DFExpr (..), DFFnRef (..), DFVar (..),
                                       LetExpr (..))
import           Ohua.Monad
import           Ohua.Types

import           Debug.Trace


type Pass m = FnName -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

passes :: (MonadOhua m, MonadError String m) => HM.HashMap FnName (Pass m)
passes =
    [ ("com.ohua.lang/smap", lowerSmap)
    , ("com.ohua.lang/if", lowerIf) -- TODO check if that "if" is actually the correct name
    , ("com.ohua.lang/seq", lowerSeq)
    ]


-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadError String m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
        case msum $ map (\a -> if HS.member a defined then Just a else Nothing) produced of
            Just b  -> throwError $ "Rebinding of " ++ show b
            Nothing -> return ()
        modify (addAll produced)

    addAll add set = foldr' HS.insert set add


-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: MonadError String m => DFExpr -> m ()
checkSSAExpr (DFExpr l _) = checkSSA l


-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: (MonadOhua m, MonadError String m) => Expression -> m DFExpr
lowerALang expr = do
    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var
  where
    go  (Var (Local bnd)) = return bnd
    go  (Var _) = throwError "Non local return binding"
    go  (Let assign expr rest) = do
        (fn, fnId, args) <- handleApplyExpr expr
        tell =<< dispatchFnType fn fnId assign args
        go rest
    go  x = throwError "Expected `let` or binding"


-- | Select a function for lowering a let expression absed on the type of the function called.
dispatchFnType :: (MonadOhua m, MonadError String m) => Pass m
dispatchFnType fn = fromMaybe lowerDefault (HM.lookup fn passes) $ fn


-- | Lowers an smap call.
lowerSmap :: (MonadOhua m, MonadError String m) => Pass m
lowerSmap _ fnId assign args = do
    -- TODO add the "one-to-n"s
    lowered <- lowerALang body
    identityId <- generateId
    coll <- expectVar collE
    pure
        $ (LetExpr fnId inVar (DFFunction "com.ohua.lang/smap-fun") (return coll) Nothing
            <| letExprs lowered)
        |> LetExpr identityId assign (EmbedSf "com.ohua.lang/collect") [DFVar (returnVar lowered)] Nothing
  where
    [Lambda inVar body, collE] = args


-- | Lowers an if call.
lowerIf :: (MonadOhua m, MonadError String m) => Pass m
lowerIf _ fnId assign args = do
    dfCond <- expectVar condition
    loweredThen <- lowerALang thenBody
    loweredElse <- lowerALang elseBody

    switchId <- generateId

    pure
        $ (LetExpr fnId (Destructure [thenVar, elseVar])
            (DFFunction "com.ohua.lang/ifThenElse") [dfCond] Nothing
            <| tieContext thenVar (letExprs loweredThen)
            >< tieContext elseVar (letExprs loweredElse))
        |> LetExpr switchId assign (DFFunction "com.ohua.lang/switch")
            [DFVar (returnVar loweredThen), DFVar (returnVar loweredElse)]
            Nothing
  where
    [condition, Lambda (Direct thenVar) thenBody, Lambda (Direct elseVar) elseBody] = args


-- | Lowers a seq call.
lowerSeq :: (MonadOhua m, MonadError String m) => Pass m
lowerSeq _ fnId assign args = do
    loweredExpr <- lowerALang toSeq
    return $ tieContext binding (letExprs loweredExpr) |> LetExpr fnId assign (EmbedSf "com.ohua.lang/id") [DFVar (returnVar loweredExpr)] Nothing
  where
    -- Is binding first the right way around?
    [Var (Local binding), Lambda _ toSeq] = args


-- | Lower any not specially treated function type.
lowerDefault :: (MonadOhua m, MonadError String m) => Pass m
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]


-- | Tie all functions which do not depend on locally bound variables via context arc
-- to a source binding.
tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = fmap go exprs
  where
    bounds = findBoundVars exprs
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }
    isBoundArg (DFVar v) = v `HS.member` bounds
    isBoundArg _         = False


-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


-- | Insert a `one-to-n` node for each free variable to scope them.
replicateFreeVars :: (MonadOhua m, MonadError String m) => Binding -> [Binding] -> Seq LetExpr -> m (Seq LetExpr)
replicateFreeVars countSource_ initialBindings exprs = do
    (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
    pure $ S.fromList replicators >< fmap (renameWith $ HM.fromList $ zip freeVars replications) exprs
  where
    boundVars = findBoundVars exprs `mappend` HS.fromList initialBindings

    freeVars = HS.toList $ HS.fromList $ concatMap (mapMaybe f . callArguments) exprs

    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing

    renameWith m e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }

    mkReplicator var = do
        id <- generateId
        newVar <- generateBindingWith var
        pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar countSource_, DFVar var] Nothing, newVar)


-- | Ananlyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: (MonadOhua m, MonadError String m) => Expression -> m (FnName, FnId, [Expression])
handleApplyExpr (Apply fn arg) = go fn [arg]
  where
    go (Var (Sf fn id)) args = (fn, , args) <$> maybe generateId return id
            -- reject algos for now
    go (Var v) _             = throwError $ "Expected Var Sf but got: Var " ++ show v -- FIXME there should be a special type of error here that takes the string and a value
    go (Apply fn arg) args   = go fn (arg:args)
    go x _                   = throwError $ "Expected Apply or Var but got: " ++ show x
handleApplyExpr (Var (Sf fn id)) = (fn, , []) <$> maybe generateId return id
handleApplyExpr g = throwError $ "Expected apply but got: " ++ show g


-- | Inspect an expression expecting something which can be captured in a DFVar otherwies throws appropriate errors.
expectVar :: MonadError String m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var _)           = throwError "Var must be local or env"
expectVar _                 = throwError "Argument must be var"
