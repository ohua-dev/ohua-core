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
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.DFLang.Passes where


import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence                   (Seq, (><))
import qualified Data.Sequence                   as S
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.DFLang.HOF as HOF
import           Ohua.DFLang.HOF.Smap
import           Ohua.DFLang.HOF.If
import           Ohua.DFLang.Lang                (DFExpr (..), DFFnRef (..),
                                                  DFVar (..), LetExpr (..))
import           Ohua.Monad
import           Ohua.Types


-- class LoweringPass a where
--   -- sfn :: String
--   pass :: a -> Seq LetExpr
--   -- scope m ::
--   -- contextify ::
--
-- data SFRef = SFRef { fname::FnName, fid::FnId, as::Assignment, es::[Expression] }
-- type Cond = SFRef
-- -- smap = SFRef { fname = FnName "com.ohua.lang" "smap" }
-- -- cond = SFRef { fname = FnName "com.ohua.lang" "if"}
--
-- instance LoweringPass SMap where
--   -- pass :: FnName -> FnId -> Assignment -> [Expression] -> Seq LetExpr
--   pass s = undefined
--
-- instance LoweringPass Cond where
--   pass i = undefined


type Pass m = FnName -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)


-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadError String m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
            f a | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b  -> throwError $ "Rebinding of " ++ show b
            Nothing -> return ()
        modify (addAll produced)

    addAll = flip $ foldr' HS.insert


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
        case HM.lookup fn hofNames of
            Just (WHOF (_ :: Proxy p)) -> lowerHOF (name :: TaggedFnName p) assign args
            Nothing       -> tell =<< lowerDefault fn fnId assign args
        go rest
    go  _ = throwError "Expected `let` or binding"


-- | Lower any not specially treated function type.
lowerDefault :: (MonadOhua m, MonadError String m) => Pass m
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]

    -- finds all functions that use vars from the lexical context and adds the context source to them.
-- needs to do the following two things
--        1. contextify all functions that do not have a bound arg -> context arg
--        2. provide lexical scope access to these args
tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = tieContext0 bounds ctxSource exprs
  where
    bounds = findBoundVars exprs


tieContext0 :: Functor f => HS.HashSet Binding -> Binding -> f LetExpr -> f LetExpr
tieContext0 bounds ctxSource = fmap go
  where
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }

    isBoundArg (DFVar v) = v `HS.member` bounds
    isBoundArg _         = False

-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


findFreeVars0 :: Foldable f => HS.HashSet Binding -> f LetExpr -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . concatMap (mapMaybe f . callArguments)
  where
    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing


-- | Insert a `one-to-n` node for each free variable to scope them.
replicateFreeVars :: (MonadOhua m, MonadError String m) => Binding -> [Binding] -> Seq LetExpr -> m (Seq LetExpr)
replicateFreeVars countSource_ initialBindings exprs = do
    (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
    pure $ S.fromList replicators >< renameWith (HM.fromList $ zip freeVars replications) exprs
  where
    boundVars = findBoundVars exprs `mappend` HS.fromList initialBindings

    freeVars = HS.toList $ HS.fromList $ concatMap (mapMaybe f . callArguments) exprs

    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing

    mkReplicator var = do
        id <- generateId
        newVar <- generateBindingWith var
        pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar countSource_, DFVar var] Nothing, newVar)


renameWith :: Functor f => HM.HashMap Binding Binding -> f LetExpr -> f LetExpr
renameWith m = fmap go
  where
    go e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }


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


lowerHOF :: forall f m . (MonadError String m, MonadOhua m, HigherOrderFunction f, MonadWriter (Seq LetExpr) m)
         => TaggedFnName f -> Assignment -> [Expression] -> m ()
lowerHOF _ assign args = do
    simpleArgs <- mapM handleArg args
    let newFnArgs = map (either Variable LamArg . fmap fst) simpleArgs
    f <- HOF.parseCallAndInitState newFnArgs :: m f
    flip evalStateT f $ do
        createContextEntry >>= tell
        let lambdas = rights simpleArgs
        for_ lambdas $ \(lam, body) -> do
            let boundVars = findBoundVars body `mappend` HS.fromList (flattenAssign $ beginAssignment lam)
            let freeVars = HS.toList $ findFreeVars0 boundVars body
            (scopers, renaming) <- scopeFreeVariables lam freeVars
            tell scopers
            scopeUnbound <- contextifyUnboundFunctions lam
            let tieContext1 = if scopeUnbound then tieContext0 boundVars (head $ flattenAssign $ beginAssignment lam) else id
            tell $ tieContext1 (renameWith (HM.fromList renaming) body)
        createContextExit assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    handleArg (Lambda assign body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign bnd, lets)
    handleArg _ = throwError "unexpected type of argument"


hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    -- , WHOF (Proxy :: Proxy SeqFn)
    ]

hofNames :: HM.HashMap FnName WHOF
hofNames = HM.fromList $ map (extractName &&& id) hofs
  where extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
