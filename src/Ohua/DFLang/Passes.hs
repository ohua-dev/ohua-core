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


import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Data.Sequence        (Seq, (<|), (><), (|>))
import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang     (DFExpr (..), DFFnRef (..), DFVar (..),
                                       LetExpr (..))
import qualified Ohua.DFLang.Lang     as DFLang
import           Ohua.Monad
import           Ohua.Types

lowerALang :: (MonadOhua m, MonadError String m) => Expression -> m DFExpr
lowerALang expr = (\(var, exprs) -> DFExpr exprs var) <$> runWriterT (go expr)
  where
    go  (Var bnd) =
        case bnd of
            Local bnd -> return bnd
            _         -> throwError "Non local return binding"
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
        -- TODO check if that "if" is actually the correct name
        "com.ohua.lang/if" -> do
            dfCond <- case condition of
                Var var ->
                    case var of
                        Local b -> return $ DFVar b
                        Env e -> return $ DFEnvVar e
                        _ -> throwError "Algo and sfref not allowed as condition"
                _ -> throwError "Expected var as condition"
            loweredThen <- lowerALang thenBody
            loweredElse <- lowerALang elseBody

            thenVar <- generateBindingWith "then"
            elseVar <- generateBindingWith "else"
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
            [condition, Lambda thenVar thenBody, Lambda elseVar elseBody] = args
        _ -> (\args' -> return $ LetExpr fnId assign (EmbedSf fn) args' Nothing) <$> mapM expectVar args


tieContext :: Binding -> Seq LetExpr -> Seq LetExpr
tieContext ctxSource exprs = fmap go exprs
  where
    bounds = findBoundVars exprs
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }
    isBoundArg (DFVar v) | v `HS.member` bounds = True
    isBoundArg _         = False


findBoundVars :: (Seq LetExpr) -> HS.HashSet Binding
findBoundVars =  HS.fromList . fold . fmap (flattenAssign . returnAssignment)


replicateFreeVars :: (MonadOhua m, MonadError String m) => Binding -> [Binding] -> [LetExpr] -> m [LetExpr]
replicateFreeVars countSource_ initialBindings exprs = do
    (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
    pure $ replicators ++ map (renameWith $ HM.fromList $ zip freeVars replications) exprs
  where
    boundVars = HS.fromList $ concat $ map (flattenAssign . returnAssignment) exprs

    freeVars = HS.toList $ HS.fromList $ concatMap (mapMaybe f . callArguments) exprs

    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing

    renameWith m e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }

    mkReplicator var = do
        id <- generateId
        newVar <- generateBindingWith var
        pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar var] Nothing, newVar)


handleApplyExpr :: (MonadOhua m, MonadError String m) => Expression -> m (FnName, FnId, [Expression])
handleApplyExpr (Apply fn arg) = go fn [arg]
  where
    go (Var var) args =
        case var of
            Sf fn id -> (fn, , args) <$> maybe generateId return id
            -- reject algos for now
            _        -> throwError "Expected Sf Var"
    go (Apply fn arg) args = go fn (arg:args)
    go _ _ = throwError "Expected Apply or Var"
handleApplyExpr _ = throwError "Expected apply"


expectVar :: MonadError String m => Expression -> m DFVar
expectVar (Var v) =
    case v of
        Local bnd -> pure $ DFVar bnd
        Env i     -> pure $ DFEnvVar i
        _         -> throwError "Var must be local or env"
expectVar _ = throwError "Argument must be var"
