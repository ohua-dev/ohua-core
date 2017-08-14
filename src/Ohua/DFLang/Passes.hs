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
{-# LANGUAGE CPP #-}
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
import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang     (DFExpr (..), DFFnRef (..), DFVar (..),
                                       LetExpr (..))
import           Ohua.Monad
import           Ohua.Types


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
        tell =<< dispatchFnType fnId assign args fn
        go rest
    go  _ = throwError "Expected `let` or binding"

dispatchFnType :: (MonadOhua m, MonadError String m)
               => FnId -> Assignment -> [Expression] -> FnName -> m (Seq LetExpr)
dispatchFnType fnId assign args fn =
    case fn of
        "com.ohua.lang/smap" -> do
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
        -- TODO check if that "if" is actually the correct name -> if not then we will rename it to be exactly this one!
        -- TODO variable scoping.
        -- TBD: should this be a concept with its on pass. then a context needs to define its own scope ops. as a result:
        -- this needs to become a type which defines these details specifically.
        "com.ohua.lang/if" -> do
            dfCond <- case condition of
                Var (Local b) -> return $ DFVar b
                Var (Env e) -> return $ DFEnvVar e
                Var _ -> throwError "Algo and sfref not allowed as condition"
                _ -> throwError "Expected var as condition"
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
        "com.ohua.lang/seq" -> undefined
        _ -> (\args' -> return $ LetExpr fnId assign (EmbedSf fn) args' Nothing) <$> mapM expectVar args


tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = fmap go exprs
  where
    bounds = findBoundVars exprs
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }
    isBoundArg (DFVar v) = v `HS.member` bounds
    isBoundArg _         = False


findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


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


handleApplyExpr :: (MonadOhua m, MonadError String m) => Expression -> m (FnName, FnId, [Expression])
handleApplyExpr (Apply fn arg) = go fn [arg]
  where
    go (Var (Sf fn id)) args = (fn, , args) <$> maybe generateId return id
            -- reject algos for now
    go (Var _) _             = throwError "Expected Sf Var"
    go (Apply fn arg) args   = go fn (arg:args)
    go _ _                   = throwError "Expected Apply or Var"
handleApplyExpr _ = throwError "Expected apply"


expectVar :: MonadError String m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var _)           = throwError "Var must be local or env"
expectVar _                 = throwError "Argument must be var"
