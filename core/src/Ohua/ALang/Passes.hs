-- |
-- Module      : $Header$
-- Description : Passes over algorithm language terms to ensure certain invariants
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- This module implements a set of passes over ALang which perform
-- various tasks. The most important function is `normalize`, which
-- transforms an arbitrary ALang expression either into the normal
-- form of a sequence of let bindings which are invocations of
-- stateful functions on local or environment variables finalised by a
-- local binding as a return value.
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.ALang.Passes where

import Ohua.Prelude

import Control.Comonad (extract)
import Control.Monad.RWS.Lazy (evalRWST)
import Control.Monad.Writer (listen, runWriter, tell)
import Data.Functor.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Ohua.ALang.Lang
import Ohua.ALang.PPrint
import Ohua.ALang.Passes.If
import Ohua.ALang.Passes.Seq
import Ohua.ALang.Passes.Smap
import Ohua.ALang.Passes.Unit
import qualified Ohua.ALang.Refs as Refs
import Ohua.Stage

runCorePasses :: MonadOhua m => Expression -> m Expression
runCorePasses expr = do
    let exprE = mkUnitFunctionsExplicit expr
    stage "unit-transformation" exprE
    smapE <- smapRewrite exprE
    -- traceM $ "after 'smap' pass:\n" <> (show $ prettyExpr smapE)
    stage "smap-transformation" smapE
    ifE <- ifRewrite smapE
    -- traceM $ "after 'if' pass:\n" <> (show $ prettyExpr ifE)
    stage "conditionals-transformation" ifE
    seqE <- seqRewrite ifE
    -- traceM $ "after 'seq' pass:\n" <> (show $ prettyExpr seqE)
    stage "seq-transformation" seqE
    return seqE

-- | Inline all references to lambdas.
-- Aka `let f = (\a -> E) in f N` -> `(\a -> E) N`
inlineLambdaRefs :: MonadOhua m => Expression -> m Expression
inlineLambdaRefs = flip runReaderT mempty . para go
  where
    go (LetF b (Lambda _ _, l) (_, body)) =
        l >>= \l' -> local (HM.insert b l') body
    go (VarF bnd) = asks (fromMaybe (Var bnd) . HM.lookup bnd)
    go e = embed <$> traverse snd e

-- | Reduce lambdas by simulating application
-- Aka `(\a -> E) N` -> `let a = N in E`
-- Assumes lambda refs have been inlined
inlineLambda :: Expression -> Expression
inlineLambda =
    cata $ \case
        e@(ApplyF func argument) ->
            case func of
                Lambda assignment body -> Let assignment argument body
                Apply _ _ -> reduceLetCWith f func
                    where f (Lambda assignment body) =
                              Let assignment argument body
                          f v0 = Apply v0 argument
                _ -> embed e
        e -> embed e

-- recursively performs the substitution
--
-- let x = (let y = M in A) in E[x] -> let y = M in let x = A in E[x]
reduceLetA :: Expression -> Expression
reduceLetA =
    \case
        Let assign (Let assign2 val expr3) expr ->
            Let assign2 val $ reduceLetA $ Let assign expr3 expr
        e -> e

reduceLetCWith :: (Expression -> Expression) -> Expression -> Expression
reduceLetCWith f =
    \case
        Apply (Let assign val expr) argument ->
            Let assign val $ reduceLetCWith f $ Apply expr argument
        e -> f e

reduceLetC :: Expression -> Expression
reduceLetC = reduceLetCWith id

reduceAppArgument :: Expression -> Expression
reduceAppArgument =
    \case
        Apply function (Let assign val expr) ->
            Let assign val $ reduceApplication $ Apply function expr
        e -> e

-- recursively performs the substitution
--
-- (let x = M in A) N -> let x = M in A N
--
-- and then
--
-- A (let x = M in N) -> let x = M in A N
reduceApplication :: Expression -> Expression
reduceApplication = reduceLetCWith reduceAppArgument

-- | Lift all nested lets to the top level
-- Aka `let x = let y = E in N in M` -> `let y = E in let x = N in M`
-- and `(let x = E in F) a` -> `let x = E in F a`
letLift :: Expression -> Expression
letLift =
    cata $ \e ->
        let f =
                case e of
                    LetF _ _ _ -> reduceLetA
                    ApplyF _ _ -> reduceApplication
                    _ -> id
         in f $ embed e

-- -- | Inline all direct reassignments.
-- -- Aka `let x = E in let y = x in y` -> `let x = E in x`
inlineReassignments :: Expression -> Expression
inlineReassignments = flip runReader HM.empty . cata go
  where
    go (LetF bnd val body) =
        val >>= \v ->
            let requestReplace = local (HM.insert bnd v) body
             in case v of
                    Var {} -> requestReplace
                    Lit {} -> requestReplace
                    _ -> Let bnd v <$> body
    go (VarF val) = asks (fromMaybe (Var val) . HM.lookup val)
    go e = embed <$> sequence e

-- | Transforms the final expression into a let expression with the result variable as body.
-- Aka `let x = E in some/sf a` -> `let x = E in let y = some/sf a in y`
--
-- EDIT: Now also does the same for any residual lambdas
ensureFinalLet :: MonadOhua m => Expression -> m Expression
ensureFinalLet = ensureFinalLetInLambdas >=> ensureFinalLet'

-- | Transforms the final expression into a let expression with the result variable as body.
ensureFinalLet' :: MonadOhua m => Expression -> m Expression
ensureFinalLet' =
    para $ \case
        LetF b (oldV, _) (_, recB) -> Let b oldV <$> recB -- Recurse only into let body, not the bound value
        any
            | isVarOrLambdaF any -> embed <$> traverse snd any -- Don't rebind a lambda or var. Continue or terminate
            | otherwise -> do -- Rebind anything else
                newBnd <- generateBinding
                pure $ Let newBnd (embed $ fmap fst any) (Var newBnd)
  where
    isVarOrLambdaF =
        \case
            VarF _ -> True
            LambdaF {} -> True
            _ -> False

-- | Obsolete, will be removed soon. Replaced by `ensureFinalLet'`
ensureFinalLet'' :: MonadOhua m => Expression -> m Expression
ensureFinalLet'' (Let a e b) = Let a e <$> ensureFinalLet' b
ensureFinalLet'' v@(Var _) = return v
    -- I'm not 100% sure about this case, perhaps this ought to be in
    -- `ensureFinalLetInLambdas` instead
ensureFinalLet'' (Lambda b body) = Lambda b <$> ensureFinalLet' body
ensureFinalLet'' a = do
    newBnd <- generateBinding
    return $ Let newBnd a (Var newBnd)

ensureFinalLetInLambdas :: MonadOhua m => Expression -> m Expression
ensureFinalLetInLambdas =
    cata $ \case
        LambdaF bnd body -> Lambda bnd <$> (ensureFinalLet' =<< body)
        a -> embed <$> sequence a

ensureAtLeastOneCall :: (Monad m, MonadGenBnd m) => Expression -> m Expression
ensureAtLeastOneCall e@(Var _) = do
    newBnd <- generateBinding
    pure $ Let newBnd (PureFunction Refs.id Nothing `Apply` e) $ Var newBnd
ensureAtLeastOneCall e = cata f e
  where
    f (LambdaF bnd body) =
        body >>= \case
            v@(Var _) -> do
                newBnd <- generateBinding
                pure $
                    Lambda bnd $
                    Let newBnd (PureFunction Refs.id Nothing `Apply` v) $
                    Var newBnd
            eInner -> pure $ Lambda bnd eInner
    f eInner = embed <$> sequence eInner

-- | Removes bindings that are never used.
-- This is actually not safe becuase sfn invocations may have side effects
-- and therefore cannot be removed.
-- Assumes ssa for simplicity
removeUnusedBindings :: Expression -> Expression
removeUnusedBindings = fst . runWriter . cata go
  where
    go (VarF val) = tell (HS.singleton val) >> return (Var val)
    go (LetF b val body) = do
        (inner, used) <- listen body
        if not $ b `HS.member` used
            then return inner
            else do
                val' <- val
                pure $ Let b val' inner
    go e = embed <$> sequence e

newtype MonoidCombineHashMap k v =
    MonoidCombineHashMap (HashMap k v)
    deriving (Show, Eq, Ord)

instance (Semigroup v, Eq k, Hashable k) =>
         Semigroup (MonoidCombineHashMap k v) where
    MonoidCombineHashMap m1 <> MonoidCombineHashMap m2 =
        MonoidCombineHashMap $ HM.unionWith (<>) m1 m2

instance (Semigroup v, Eq k, Hashable k) =>
         Monoid (MonoidCombineHashMap k v) where
    mempty = MonoidCombineHashMap mempty

data WasTouched
    = No
    | Yes
    deriving (Show, Eq, Ord)

instance Semigroup WasTouched where
    (<>) = max

instance Monoid WasTouched where
    mempty = No

type TouchMap = MonoidCombineHashMap Binding (WasTouched, WasTouched)

wasTouchedAsFunction :: Binding -> TouchMap
wasTouchedAsFunction bnd = MonoidCombineHashMap $ HM.singleton bnd (Yes, No)

wasTouchedAsValue :: Binding -> TouchMap
wasTouchedAsValue bnd = MonoidCombineHashMap $ HM.singleton bnd (No, Yes)

lookupTouchState :: Binding -> TouchMap -> (WasTouched, WasTouched)
lookupTouchState bnd (MonoidCombineHashMap m) =
    fromMaybe mempty $ HM.lookup bnd m

-- | Reduce curried expressions.  aka `let f = some/sf a in f b`
-- becomes `some/sf a b`.  It both inlines the curried function and
-- removes the binding site.  Recursively calls it self and therefore
-- handles redefinitions as well.  It only substitutes vars in the
-- function positions of apply's hence it may produce an expression
-- with undefined local bindings.  It is recommended therefore to
-- check this with 'noUndefinedBindings'.  If an undefined binding is
-- left behind which indicates the source expression was not
-- fulfilling all its invariants.
removeCurrying ::
       forall m. MonadError Error m
    => Expression
    -> m Expression
removeCurrying e = fst <$> evalRWST (para inlinePartials e) mempty ()
  where
    inlinePartials (LetF bnd (_, val) (_, body)) = do
        val' <- val
        (body', touched) <- listen $ local (HM.insert bnd val') body
        case lookupTouchState bnd touched of
            (Yes, Yes) ->
                throwErrorDebugS $
                "Binding was used as function and value " <> show bnd
            (Yes, _) -> pure body'
            _ -> pure $ Let bnd val' body'
    inlinePartials (ApplyF (Var bnd, _) (_, arg)) = do
        tell $ wasTouchedAsFunction bnd
        val <- asks (HM.lookup bnd)
        Apply <$>
            (maybe
                 (failWith $ "No suitable value found for binding " <> show bnd)
                 pure
                 val) <*>
            arg
    inlinePartials (VarF bnd) = tell (wasTouchedAsValue bnd) >> pure (Var bnd)
    inlinePartials innerExpr = embed <$> traverse snd innerExpr

-- | Ensures the expression is a sequence of let statements terminated
-- with a local variable.
hasFinalLet :: MonadOhua m => Expression -> m ()
hasFinalLet =
    cata $ \case
        LetF _ _ body -> body
        VarF {} -> return ()
        _ -> failWith "Final value is not a var"

-- | Ensures all of the optionally provided stateful function ids are unique.
noDuplicateIds :: MonadError Error m => Expression -> m ()
noDuplicateIds = flip evalStateT mempty . cata go
  where
    go (PureFunctionF _ (Just funid)) = do
        isMember <- gets (HS.member funid)
        when isMember $ failWith $ "Duplicate id " <> show funid
        modify (HS.insert funid)
    go e = sequence_ e

-- | Checks that no apply to a local variable is performed.  This is a
-- simple check and it will pass on complex expressions even if they
-- would reduce to an apply to a local variable.
applyToPureFunction :: MonadOhua m => Expression -> m ()
applyToPureFunction =
    para $ \case
        ApplyF (Var bnd, _) _ ->
            failWith $ "Illegal Apply to local var " <> show bnd
        e -> sequence_ $ fmap snd e

-- | Checks that all local bindings are defined before use.
-- Scoped. Aka bindings are only visible in their respective scopes.
-- Hence the expression does not need to be in SSA form.
noUndefinedBindings :: MonadOhua m => Expression -> m ()
noUndefinedBindings = flip runReaderT mempty . cata go
  where
    go (LetF b val body) = val >> registerBinding b body
    go (VarF bnd) = do
        isDefined <- asks (HS.member bnd)
        unless isDefined $ failWith $ "Not in scope " <> show bnd
    go (LambdaF b body) = registerBinding b body
    go e = sequence_ e
    registerBinding = local . HS.insert

checkProgramValidity :: MonadOhua m => Expression -> m ()
checkProgramValidity e = do
    hasFinalLet e
    noDuplicateIds e
    applyToPureFunction e
    noUndefinedBindings e

-- | Lifts something like @if (f x) a b@ to @let x0 = f x in if x0 a b@
liftApplyToApply :: MonadOhua m => Expression -> m Expression
liftApplyToApply =
    lrPrewalkExprM $ \case
        Apply fn arg@(Apply _ _) -> do
            bnd <- generateBinding
            return $ Let bnd arg $ Apply fn (Var bnd)
        a -> return a

-- normalizeBind :: (MonadError Error m, MonadGenBnd m) => Expression -> m Expression
-- normalizeBind =
--     rewriteM $ \case
--         BindState e2 e1@(PureFunction _ _) ->
--             case e2 of
--                 Var _ -> pure Nothing
--                 Lit _ -> pure Nothing
--                 _ ->
--                     generateBinding >>= \b ->
--                         pure $ Just $ Let b e2 (BindState (Var b) e1)
--         BindState _ _ -> throwError "State bind target must be a pure function reference"
--         _ -> pure Nothing
dumpNormalizeDebug = False

putStrLnND :: (Print str, MonadIO m) => str -> m ()
putStrLnND =
    if dumpNormalizeDebug
        then putStrLn
        else const $ return ()

printND :: (Show a, MonadIO m) => a -> m ()
printND =
    if dumpNormalizeDebug
        then print
        else const $ return ()

-- The canonical composition of the above transformations to create a
-- program with the invariants we expect.
normalize :: MonadOhua m => Expression -> m Expression
normalize e =
    reduceLambdas (letLift e) >>=
    (\a ->
         putStrLnND ("Reduced lamdas" :: Text) >> printND (pretty a) >> return a) >>=
    return . inlineReassignments >>=
    removeCurrying >>=
    (\a ->
         putStrLnND ("Removed Currying" :: Text) >> printND (pretty a) >>
         return a) >>=
    liftApplyToApply >>=
    (\a -> putStrLnND ("App to App" :: Text) >> printND (pretty a) >> return a) .
    letLift >>=
    ensureFinalLet . inlineReassignments >>=
    ensureAtLeastOneCall
    -- we repeat this step until a fix point is reached.
    -- this is necessary as lambdas may be input to lambdas,
    -- which means after inlining them we may be able again to
    -- inline a ref and then inline the lambda.
    -- I doubt this will ever do more than two or three iterations,
    -- but to make sure it accepts every valid program this is necessary.
  where
    reduceLambdas expr = do
        res <- letLift . inlineLambda <$> inlineLambdaRefs expr
        if res == expr
            then return res
            else reduceLambdas res
-- letLift (Let assign1 (Let assign2 expr2 expr3) expr4) = letLift $ Let assign2 expr2 $ Let assign1 expr3 expr4
-- letLift (Let assign v@(Var _) expr) = Let assign v $ letLift expr
-- letLift (Let assign val expr) =
--     case letLift val of
--         v'@(Let _ _ _) -> letLift $ Let assign v' expr
--         _ -> Let assign v' $ letLift expr
-- letLift e@(Var _) = e
-- letLift (Apply v@(Var _) argument) = Apply v (letLift argument)
-- letLift (Apply (Let assign expr function) argument) = letLift $ Let assign expr $ Apply function argument
-- letLift (Apply function argument) =
--     case letLift argument of
