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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}
module Ohua.DFLang.Passes where


import           Control.Arrow
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
--import           Control.Monad.Trans.Reader
import           Control.Monad
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Sequence              (Seq, (><), (|>))
import qualified Data.Sequence              as S
import           Data.Text                  (pack)
import           Lens.Micro
import           Ohua.ALang.Lang
import qualified Ohua.ALang.Refs            as ALangRefs
import           Ohua.DFLang.HOF            as HOF
import           Ohua.DFLang.HOF.If
import           Ohua.DFLang.HOF.Seq
import           Ohua.DFLang.HOF.Smap
import           Ohua.DFLang.Lang           (DFExpr (..), DFFnRef (..),
                                             DFVar (..), LetExpr (..))
import qualified Ohua.DFLang.Refs           as Refs
import           Ohua.DFLang.TailRec        (RecursiveLambdaSpec,
                                             lowerRecAlgoCall,
                                             recursionLowering)
import           Ohua.DFLang.Util
import           Ohua.Monad                 as M
import           Ohua.Types                 as Ty
import           Ohua.Util
import qualified Ohua.Util.Str              as Str

type Pass m = QualifiedBinding -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

--data AlgoSpec m = AlgoSpec { formalInputVars :: [Binding],
--                             dfExpr :: m DFExpr }

newtype LetRec = LetRec { unLetRec :: HM.HashMap Binding (Assignment, Expression, Binding) }


traceState :: (Member (Reader LetRec) effs, Eff effs ~ m)
           => m a -> m a
traceState cont = (flip trace cont . ("State: " ++) . show . HM.keys . unLetRec) =<< ask

logState :: Members '[Logger, Reader LetRec] effs => Eff effs ()
logState = ask >>= logDebugN . ("Current state: " <>) . showT . HM.keys . unLetRec


-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, Ohua effs) => f LetExpr -> Eff effs ()
checkSSA = evalState (mempty :: HS.HashSet Binding) . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
            f a | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b  -> failWith $ "Rebinding of " <> Str.showS b
            Nothing -> return ()
        modify (addAll produced)

    addAll = flip $ foldr' HS.insert


-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: Ohua effs => DFExpr -> Eff effs ()
checkSSAExpr (DFExpr l _) = checkSSA l

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: Ohua effs => Expression -> Eff effs DFExpr
lowerALang expr = do
    logDebugN $ "Lowering alang expr: " <> showT expr
    (var, exprs) <- runWriter $ runReader (LetRec mempty) (lowerToDF expr)
--    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var

lowerToDF :: (Ohua effs, Members '[Writer (Seq LetExpr), Reader LetRec] effs)
          => Expression -> Eff effs Binding
lowerToDF (Var (Local bnd)) = pure bnd
lowerToDF (Var v) = failWith $ "Non local return binding: " <> Str.showS v
lowerToDF (Let assign expr rest) = do
    logDebugN "Lowering Let -->"
    logState
    handleDefinitionalExpr assign expr continuation
  where
    continuation = lowerToDF rest
lowerToDF g = failWith $ "Expected `let` or binding: " <> Str.showS g

handleDefinitionalExpr :: (Ohua effs, Members '[Writer (Seq LetExpr), Reader LetRec] effs, Eff effs ~ m) =>
                          Assignment -> Expression -> m Binding -> m Binding
handleDefinitionalExpr assign l@(Lambda arg expr) cont = do
    -- TODO handle lambdas with multiple arguments -> this requires some ALang transformation to always get the form Lambda a Lambda b ...
    handleTailRec <- asks (^. options . transformRecursiveFunctions)
    unless handleTailRec $
        failWith "Handling recursive functions is not enabled, if you want to enable this experimental feature set `transformRecursiveFunctions` to true in the options passed to the compiler."
    b <- case assign of
            Recursive b -> pure b
            x -> failWith $ "Invariant broken. Assignment should be a 'letrec' but was: " <> Str.showS x
    singleArg <- case arg of
                    Direct b -> pure b
                    x -> failWith $ "Invariant broken. Assignment should be a 'Direct' but was: " <> Str.showS x

    -- let continuation = recursionLowering [singleArg] =<< lowerLambdaExpr assign expr

    -- execute the rest of the traversal (the continuation) in the new LetRecT environment
    local (LetRec . HM.insert b (assign, expr, singleArg) . unLetRec) cont
handleDefinitionalExpr assign l@(Apply _ _) cont = do
    (fn, fnId, args) <- handleApplyExpr l
    case HM.lookup fn hofNames of
        Just (WHOF (_ :: Proxy p)) -> lowerHOF (name :: TaggedFnName p) assign args
        Nothing                    -> tell =<< lowerDefault fn fnId assign args
    cont
handleDefinitionalExpr _ e _ = failWith $ "Definitional expressions in a let can only be 'apply' or 'lambda' but got: " <> Str.showS e

-- | Lower any not specially treated function type.
lowerDefault :: Ohua effs => Pass (Eff effs)
lowerDefault "ohua.lang/recur" fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign Refs.recur args' Nothing]
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]

-- | Analyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: (Ohua effs, Members '[Writer (Seq LetExpr), Reader LetRec] effs)
                => Expression -> Eff effs (QualifiedBinding, FnId, [Expression])
handleApplyExpr l@(Apply fn arg) = ask >>= go l [] . unLetRec
  where
    go ve@(Var v) args recAlgos =
        case v of
            Sf fn id -> (fn, , args) <$> maybe generateId return id
            Local bnd
                | Just (assign, expr, singleArg) <- HM.lookup bnd recAlgos ->
                  lowerLambdaExpr assign expr
                  >>= recursionLowering [singleArg]
                  >>= lowerRecAlgoCall lowerDefault args
                | otherwise ->
                    asks (^. options . callLocalFunction) >>= \case
                        Nothing -> failWith "Calling local functions is not supported in this adapter"
                        Just fn -> (fn, , ve : args) <$> generateId
            Env _ ->
                asks (^. options . callEnvExpr) >>= \case
                    Nothing -> failWith "Calling environment functions is not supported in this adapter"
                    Just fn -> (fn, , ve : args) <$> generateId
    go (Apply fn arg) args recAlgos = go fn (arg:args) recAlgos
    go x _ _ = failWith $ "Expected Apply or Var but got: " <> Str.showS x

handleApplyExpr (Var (Sf fn id)) = (fn, , []) <$> maybe generateId return id -- what is this?
handleApplyExpr g                = failWith $ "Expected apply but got: " <> Str.showS g


-- | Analyze a lambda expression. Since we perform lambda inlining, this can only be a letrec.
lowerLambdaExpr :: Ohua effs => Assignment -> Expression -> Eff effs DFExpr
lowerLambdaExpr (Recursive binding) expr = lowerALang expr
lowerLambdaExpr a _ = failWith $ "Expression was not inlined but assignment is not a 'letrec': " <> Str.showS a


tieContext0 :: (Monad m, Functor f, Monoid (f LetExpr), Foldable f)
            => m (Maybe (f LetExpr, Binding))
            -> f LetExpr
            -> m (f LetExpr)
tieContext0 initExpr lets
    | all hasLocalArcs lets = pure lets
    | otherwise = initExpr <&> maybe lets
        (\(initExprs, scopeBnd) -> initExprs <> fmap (go scopeBnd) lets)
  where
    hasLocalArcs e
        | any isLocalArc (callArguments e) = True
        | Just ctxArg <- contextArg e
        , isLocalArc (DFVar ctxArg) = True
    hasLocalArcs _ = False

    go _ e         | hasLocalArcs e = e
    go ctxSource e = e { contextArg = Just ctxSource }

    isLocalArc (DFVar _) = True
    isLocalArc _         = False


-- | Inspect an expression expecting something which can be captured in a DFVar otherwise throws appropriate errors.
expectVar :: Member (M.Error Ty.Error) effs => Expression -> Eff effs DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var v)           = failWith $ "Var must be local or env, was " <> Str.showS v
expectVar a                 = failWith $ "Argument must be var, was " <> Str.showS a


lowerHOF :: forall f effs . (Ohua effs, HigherOrderFunction f, Member (Writer (Seq LetExpr)) effs)
         => TaggedFnName f -> Assignment -> [Expression] -> Eff effs ()
lowerHOF _ assign args = do
    simpleArgs <- mapM handleArg args
    let newFnArgs = map (either Variable LamArg . fmap fst) simpleArgs
    f <- HOF.parseCallAndInitState newFnArgs
    evalState (f :: f) $ do
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
            tell =<< tieContext0 (contextifyUnboundFunctions lam) (renameWith (HM.fromList renaming) body)
        createContextExit assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    handleArg (Lambda assign body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign bnd, lets)
    handleArg a = failWith $ "unexpected type of argument, expected var or lambda, got " <> Str.showS a


hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    , WHOF (Proxy :: Proxy SeqFn)
    ]

hofNames :: HM.HashMap QualifiedBinding WHOF
hofNames = HM.fromList $ map (extractName &&& id) hofs
  where extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
