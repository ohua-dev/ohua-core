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

import Protolude

import Control.Arrow
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Sequence (Seq)

import Ohua.ALang.Lang
import Ohua.DFLang.HOF as HOF
import Ohua.DFLang.HOF.If
import Ohua.DFLang.HOF.Seq
import Ohua.DFLang.HOF.Smap
import Ohua.DFLang.HOF.SmapG
import Ohua.DFLang.HOF.Generate
import Ohua.DFLang.Lang (DFExpr(..), DFFnRef(..), DFVar(..), LetExpr(..))
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.TailRec (lowerRecAlgoCall, recursionLowering)
import Ohua.DFLang.Util
import Ohua.Monad
import Ohua.Types
import Ohua.Util

type Pass m
     = QualifiedBinding -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

--data AlgoSpec m = AlgoSpec { formalInputVars :: [Binding],
--                             dfExpr :: m DFExpr }
newtype LetRec = LetRec
    { unLetRec :: HM.HashMap Binding (Assignment, Expression, Binding)
    }

type LetRecT m a = ReaderT LetRec m a


logState :: MonadLogger m => LetRecT m ()
logState =
    ask >>= logDebugN . ("Current state: " <>) . show . HM.keys . unLetRec

-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadOhua envExpr m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = extractBindings (returnAssignment le)
            f a
                | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b -> failWith $ "Rebinding of " <> show b
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
    logDebugN $ "Lowering alang expr: " <> showT expr
    (var, exprs) <- runWriterT $ runReaderT (lowerToDF expr) (LetRec mempty)
    return $ DFExpr exprs var

--    (var, exprs) <- runWriterT (go expr)
lowerToDF ::
       (MonadOhua env m, MonadWriter (Seq LetExpr) m)
    => Expression
    -> LetRecT m Binding
lowerToDF (Var (Local bnd)) = pure bnd
lowerToDF (Var v) = failWith $ "Non local return binding: " <> show v
lowerToDF (Let assign expr rest) = do
    logDebugN "Lowering Let -->"
    logState
    handleDefinitionalExpr assign expr continuation
  where
    continuation = lowerToDF rest
lowerToDF g = failWith $ "Expected `let` or binding: " <> show g

handleDefinitionalExpr ::
       (MonadOhua env m, MonadWriter (Seq LetExpr) m)
    => Assignment
    -> Expression
    -> LetRecT m Binding
    -> LetRecT m Binding
handleDefinitionalExpr assign (Lambda arg expr) cont
    -- TODO handle lambdas with multiple arguments -> this requires
    -- some ALang transformation to always get the form Lambda a
    -- Lambda b ...
 = do
    handleTailRec <- fromEnv (options . transformRecursiveFunctions)
    unless handleTailRec $
        failWith
            "Handling recursive functions is not enabled, if you want to enable this experimental feature set `transformRecursiveFunctions` to true in the options passed to the compiler."
    b <-
        case assign of
            Recursive b' -> pure b'
            x ->
                failWith $
                "Invariant broken. Assignment should be a 'letrec' but was: " <>
                show x
    singleArg <-
        case arg of
            Direct b' -> pure b'
            x ->
                failWith $
                "Invariant broken. Assignment should be a 'Direct' but was: " <>
                show x
    -- execute the rest of the traversal (the continuation) in the new
    -- LetRecT environment
    local (LetRec . HM.insert b (assign, expr, singleArg) . unLetRec) cont
handleDefinitionalExpr assign l@(Apply _ _) cont = do
    (fn, fnId, args) <- handleApplyExpr l
    case HM.lookup fn hofNames of
        Just (WHOF (_ :: Proxy p)) ->
            lift $ lowerHOF (name :: TaggedFnName p) assign args
        Nothing -> lift $ tell =<< lowerDefault fn fnId assign args
    cont
handleDefinitionalExpr _ e _ =
    failWith $
    "Definitional expressions in a let can only be 'apply' or 'lambda' but got: " <>
    show e

-- | Lower any not specially treated function type.
lowerDefault :: MonadOhua env m => Pass m
lowerDefault "ohua.lang/recur" fnId assign args =
    mapM expectVar args <&> \args' ->
        [LetExpr fnId assign Refs.recur args' Nothing]
lowerDefault fn fnId assign args =
    mapM expectVar args <&> \args' ->
        [LetExpr fnId assign (EmbedSf fn) args' Nothing]

-- | Analyze an apply expression, extracting the inner stateful
-- function and the nested arguments as a list.  Also generates a new
-- function id for the inner function should it not have one yet.
handleApplyExpr ::
       (MonadOhua env m, MonadWriter (Seq LetExpr) m)
    => Expression
    -> LetRecT m (QualifiedBinding, FnId, [Expression])
handleApplyExpr l@(Apply _ _) = ask >>= go l [] . unLetRec
  where
    go ve@(Var v) args recAlgos =
        case v of
            Sf fn fnId -> (fn, , args) <$> maybe generateId return fnId
            Local bnd
                | Just (assign, expr, singleArg) <- HM.lookup bnd recAlgos ->
                    lowerLambdaExpr assign expr >>=
                    recursionLowering [singleArg] >>=
                    lowerRecAlgoCall lowerDefault args
                | otherwise ->
                    fromEnv (options . callLocalFunction) >>= \case
                        Nothing ->
                            failWith
                                "Calling local functions is not supported in this adapter"
                        Just fn -> (fn, , ve : args) <$> generateId
            Env _ ->
                fromEnv (options . callEnvExpr) >>= \case
                    Nothing ->
                        failWith
                            "Calling environment functions is not supported in this adapter"
                    Just fn -> (fn, , ve : args) <$> generateId
    go (Apply fn arg) args recAlgos = go fn (arg : args) recAlgos
    go x _ _ = failWith $ "Expected Apply or Var but got: " <> show x
handleApplyExpr (Var (Sf fn fnId)) = (fn, , []) <$> maybe generateId return fnId
                                                                                 -- what is this?
handleApplyExpr g = failWith $ "Expected apply but got: " <> show g

-- | Analyze a lambda expression. Since we perform lambda inlining,
-- this can only be a letrec.
lowerLambdaExpr :: MonadOhua env m => Assignment -> Expression -> m DFExpr
lowerLambdaExpr (Recursive _) expr = lowerALang expr
lowerLambdaExpr a _ =
    failWith $
    "Expression was not inlined but assignment is not a 'letrec': " <>
    show a

tieContext0 ::
       (Monad m, Functor f, Semigroup (f LetExpr), Foldable f)
    => m (Maybe (f LetExpr, Binding))
    -> f LetExpr
    -> m (f LetExpr)
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
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i)) = pure $ DFEnvVar i
expectVar (Var v) = failWith $ "Var must be local or env, was " <> show v
expectVar a = failWith $ "Argument must be var, was " <> show a

lowerHOF ::
       forall f m envExpr.
       (MonadOhua envExpr m, HigherOrderFunction f, MonadWriter (Seq LetExpr) m)
    => TaggedFnName f
    -> Assignment
    -> [Expression]
    -> m ()
lowerHOF _ assign args = do
    simpleArgs <- mapM handleArg args
    let newFnArgs = map (either Variable LamArg . fmap fst) simpleArgs
    f <- HOF.parseCallAndInitState newFnArgs :: m f
    flip evalStateT f $ do
        createContextEntry >>= tell
        let lambdas = rights simpleArgs
        for_ lambdas $ \(lam, body) -> do
            let boundVars =
                    findBoundVars body `mappend`
                    HS.fromList (extractBindings $ beginAssignment lam)
            let freeVars = HS.toList $ findFreeVars0 boundVars body
            (scopers, renaming) <-
                if null freeVars
                    then return ([], [])
                    else scopeFreeVariables lam freeVars
            tell scopers
            tell =<<
                tieContext0
                    (contextifyUnboundFunctions lam)
                    (renameWith (HM.fromList renaming) body)
        createContextExit assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    handleArg (Lambda assign' body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign' bnd, lets)
    handleArg a =
        failWith $
        "unexpected type of argument, expected var or lambda, got " <>
        show a

hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    , WHOF (Proxy :: Proxy SeqFn)
    , WHOF (Proxy :: Proxy SmapGFn)
    , WHOF (Proxy :: Proxy GenFn)
    ]

hofNames :: HM.HashMap QualifiedBinding WHOF
hofNames = HM.fromList $ map (extractName &&& identity) hofs
  where
    extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
