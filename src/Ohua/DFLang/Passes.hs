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
import           Control.Monad.Reader
import           Control.Monad.Writer
--import           Control.Monad.Trans.Reader
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence        (Seq, (><), (|>))
import qualified Data.Sequence        as S
import           Data.Text            (pack)
import           Lens.Micro
import           Ohua.ALang.Lang
import qualified Ohua.ALang.Refs      as ALangRefs
import qualified Ohua.DFLang.Refs     as Refs
import           Ohua.DFLang.TailRec  (lowerRecAlgoCall, recursionLowering, RecursiveLambdaSpec)
import           Ohua.DFLang.HOF      as HOF
import           Ohua.DFLang.HOF.If
import           Ohua.DFLang.HOF.Seq
import           Ohua.DFLang.HOF.Smap
import           Ohua.DFLang.Lang     (DFExpr (..), DFFnRef (..), DFVar (..),
                                       LetExpr (..))
import           Ohua.DFLang.Util
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util

import           Debug.Trace

type Pass m = QualifiedBinding -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

--data AlgoSpec m = AlgoSpec { formalInputVars :: [Binding],
--                             dfExpr :: m DFExpr }

newtype LetRec m = LetRec { unLetRec :: HM.HashMap Binding (m RecursiveLambdaSpec) }

type LetRecT m a = ReaderT (LetRec m) m a

traceState :: (MonadOhua m, MonadWriter (Seq LetExpr) m) => LetRecT m a -> LetRecT m a
traceState cont = (flip trace cont . ("State: " ++) . show . HM.keys . unLetRec) =<< ask

-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadOhua envExpr m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
            f a | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b  -> failWith $ "Rebinding of " <> showT b
            Nothing -> return ()
        modify (addAll produced)

    addAll = flip $ foldr' HS.insert


-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: MonadOhua envExpr m => DFExpr -> m ()
checkSSAExpr (DFExpr l _) = checkSSA l

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: MonadOhua envExpr m => Expression -> m DFExpr
lowerALang expr = do
    (var, exprs) <- runWriterT $ (flip runReaderT (LetRec mempty) . lowerToDF) $ flip trace expr $ "ALang expression to lower: " ++ show expr
--    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var
  where
    go  (Var (Local bnd)) = return bnd
    go  (Var _) = failWith "Non local return binding"
    go  (Let assign expr rest) = do
        (fn, fnId, args) <- handleApplyExpr expr
        case HM.lookup fn hofNames of
            Just (WHOF (_ :: Proxy p)) -> lowerHOF (name :: TaggedFnName p) assign args
            Nothing       -> tell =<< lowerDefault fn fnId assign args
        go rest
    go  e = failWith $ "Expected `let` or binding, got " <> showT e

lowerToDF :: (MonadOhua m, MonadWriter (Seq LetExpr) m) => Expression -> LetRecT m Binding
lowerToDF (Var (Local bnd)) = return bnd
lowerToDF (Var v) = lift $ failWith $ pack $ "Non local return binding: " ++ show v
lowerToDF (Let assign expr rest) = trace "Lowering Let -->" $ traceState $ handleDefinitionalExpr assign expr continuation
  where
    continuation = traceState $ lowerToDF rest
lowerToDF g = lift $ failWith $ pack $ "Expected `let` or binding: " ++ show g

handleDefinitionalExpr :: (MonadOhua m, MonadWriter (Seq LetExpr) m) =>
                          Assignment -> Expression -> LetRecT m Binding -> LetRecT m Binding
handleDefinitionalExpr assign l@(Lambda arg expr) cont = do
    -- TODO handle lambdas with multiple arguments -> this requires some ALang transformation to always get the form Lambda a Lambda b ...
    let unRecursive x = case x of { Recursive b -> b;
                                              _ -> error $ "Invariant broken. Assignment should be a 'letrec' but was: " ++ show x}
    let unDirect x = case x of { Direct b -> b;
                                        _ -> error $ " Invariant broken. Assignment should be a 'Direct' but was: " ++ show x}
    let continuation = recursionLowering [unDirect arg] =<< lowerLambdaExpr assign expr

    -- execute the rest of the traversal (the continuation) in the new LetRecT environment
    local (LetRec . HM.insert (unRecursive assign) continuation . unLetRec) cont
handleDefinitionalExpr assign l@(Apply _ _) cont = do
    let go (fn, fnId, args) = case HM.lookup fn hofNames of
                                   Just (WHOF (_ :: Proxy p)) -> lift $ lowerHOF (name :: TaggedFnName p) assign args
                                   Nothing                    -> lift $ tell =<< lowerDefault fn fnId assign args
    go =<< trace ("lowering APPLY: " ++ show l) (handleApplyExpr assign l)
    cont
handleDefinitionalExpr _ e _ = lift $ failWith $ pack $ "Definitional expressions in a let can only be 'apply' or 'lambda' but got: " ++ show e

-- | Lower any not specially treated function type.
lowerDefault :: MonadOhua m => Pass m
lowerDefault "ohua.lang/recur" fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign Refs.recur args' Nothing]
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]

-- | Analyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: (MonadOhua m, MonadWriter (Seq LetExpr) m) =>
                   Assignment -> Expression -> LetRecT m (FnName, FnId, [Expression])
handleApplyExpr assign l@(Apply fn arg) =
  let
    certainly = fromMaybe $ error "Invariant broken"
    unwrapVar x = case x of { (Local b) -> b; g -> error $ "UnVar: Invariant broken: " ++ show g; }
    go (Var (Sf fn id))               args _        = (fn, , args) <$> maybe generateId return id
            -- reject algos for now
    go l@(Var v)                      args recAlgos =
          if HM.member (unwrapVar v) recAlgos then lowerRecAlgoCall lowerDefault args assign =<< lift (certainly $ HM.lookup (unwrapVar v) recAlgos) -- Justus, you should be proud of me for this line ;)
                                              else failWith $ pack $ trace ("--> recAlgos" ++ show (HM.keys recAlgos)) $ "Expected Var Sf but got: " ++ show l
    go (Apply fn arg) args recAlgos | case arg of { Var _ -> True; Lambda _ _  -> True; _ -> False} = go fn (arg:args) recAlgos
    go (Apply fn arg)                 args _        = failWith$ pack $ "Arg to apply should have been reduced to Var, EnvVar or Lambda before df lowering. Found: " ++ show arg
    go x                              _    _        = failWith $ pack $ "Expected Apply or Var but got: " ++ show x in
    do
      recAlgos <- ask
      go l [] $ unLetRec recAlgos
handleApplyExpr _ (Var (Sf fn id))  = (fn, , []) <$> maybe generateId return id -- what is this?
handleApplyExpr _ g                 = lift $ failWith $ pack $ "Expected apply but got: " ++ show g


-- | Analyze a lambda expression. Since we perform lambda inlining, this can only be a letrec.
lowerLambdaExpr :: (MonadOhua m) => Assignment -> Expression -> m DFExpr
lowerLambdaExpr (Recursive binding) expr = lowerALang expr
lowerLambdaExpr a _ = failWith $ pack $ "Expression was not inlined but assignment is not a 'letrec': " ++ show a

-- | Inspect an expression expecting something which can be captured in a DFVar otherwies throws appropriate errors.
expectVar :: MonadOhua m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var _)           = failWith "Var must be local or env"
expectVar _                 = failWith "Argument must be var"

-- FIXME is this function still used? if not delete it!
-- finds all functions that use vars from the lexical context and adds the context source to them.
-- needs to do the following two things
--        1. contextify all functions that do not have a bound arg -> context arg
--        2. provide lexical scope access to these args
tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = tieContext0 bounds ctxSource exprs
  where
    bounds = findBoundVars exprs


tieContext0 :: Functor f => Binding -> f LetExpr -> f LetExpr
tieContext0 ctxSource = fmap go
  where
    go e | any isLocalArc (callArguments e) = e
         | Just ctxArg <- contextArg e
         , isLocalArc (DFVar ctxArg) = e
    go e = e { contextArg = Just ctxSource }

    isLocalArc (DFVar _) = True
    isLocalArc _         = False

-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


findFreeVars0 :: Foldable f => HS.HashSet Binding -> f LetExpr -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . concatMap (mapMaybe f . callArguments)
  where
    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing


renameWith :: Functor f => HM.HashMap Binding Binding -> f LetExpr -> f LetExpr
renameWith m = fmap go
  where
    go e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }


-- | Ananlyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: MonadOhua envExpr m => Expression -> m (QualifiedBinding, FnId, [Expression])
handleApplyExpr (Apply fn arg) = go fn [arg]
  where
    go ve@(Var v) args =
        case v of
            Sf fn id -> (fn, , args) <$> maybe generateId return id
            Local _ ->
                fromEnv (options . callLocalFunction) >>= \case
                    Nothing -> failWith "Calling local functions is not supported in this adapter"
                    Just fn -> (fn, , ve : args) <$> generateId
            Env _ ->
                fromEnv (options . callEnvExpr) >>= \case
                    Nothing -> failWith "Calling environment functions is not supported in this adapter"
                    Just fn -> (fn, , ve : args) <$> generateId
    go (Apply fn arg) args   = go fn (arg:args)
    go x _                   = failWith $ "Expected Apply or Var but got: " <> showT x
handleApplyExpr (Var (Sf fn id)) = (fn, , []) <$> maybe generateId return id
handleApplyExpr g = failWith $ "Expected apply but got: " <> showT g


-- | Inspect an expression expecting something which can be captured in a DFVar otherwise throws appropriate errors.
expectVar :: MonadOhua envExpr m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var v)           = failWith $ "Var must be local or env, was " <> showT v
expectVar a                 = failWith $ "Argument must be var, was " <> showT a


lowerHOF :: forall f m envExpr . (MonadOhua envExpr m, HigherOrderFunction f, MonadWriter (Seq LetExpr) m)
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
            (scopers, renaming) <-
                if null freeVars then
                    return ([], [])
                else
                    scopeFreeVariables lam freeVars
            tell scopers
            scopeUnbound <- contextifyUnboundFunctions lam
            let tieContext1 = case scopeUnbound of
                    Just (initExpr, bnd) -> (initExpr <>) . tieContext0 bnd
                    Nothing  -> id
            tell $ tieContext1 (renameWith (HM.fromList renaming) body)
        createContextExit assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    handleArg (Lambda assign body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign bnd, lets)
    handleArg a = failWith $ "unexpected type of argument, expected var or lambda, got " <> showT a


hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    , WHOF (Proxy :: Proxy SeqFn)
    ]

hofNames :: HM.HashMap QualifiedBinding WHOF
hofNames = HM.fromList $ map (extractName &&& id) hofs
  where extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
