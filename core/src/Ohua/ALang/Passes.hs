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
import qualified Ohua.ALang.Refs as Refs
import Ohua.Stage

runCorePasses :: MonadOhua m => Expression -> m Expression
runCorePasses expr = do
    smapE <- smapRewrite expr
    -- traceM $ "after 'smap' pass:\n" <> (show $ prettyExpr smapE)
    stage "smap-transformation" smapE
    ifE <- ifRewrite smapE
    -- traceM $ "after 'if' pass:\n" <> (show $ prettyExpr ifE)
    stage "conditionals-transformation" ifE
    seqE <- seqRewrite ifE
    -- traceM $ "after 'seq' pass:\n" <> (show $ prettyExpr seqE)
    stage "seq-transformation" seqE
    return seqE

-- -- | Inline all references to lambdas.
-- -- Aka `let f = (\a -> E) in f N` -> `(\a -> E) N`
-- inlineLambdaRefs ::
--        (MonadState (HM.HashMap Binding Expr) m)
--     => Expression
--     -> m (Maybe Expression)
-- inlineLambdaRefs = \case
--     Let b l@Lambda{} body -> do
--         modify (HM.insert b l)
--         pure $ Just body
--     Var bnd -> gets (HM.lookup bnd)
--     _ -> pure Nothing
floatOutLet :: Expression -> Maybe Expression
floatOutLet =
    \case
        Apply fun arg
            | Let b e fun' <- fun -> Just $ Let b e (Apply fun' arg)
            | Let b e arg' <- arg -> Just $ Let b e (Apply fun arg')
        Let b (Let b2 e2 e3) e -> Just $ Let b2 e2 $ Let b e3 e
        BindState fun arg
            | Let b e fun' <- fun -> Just $ Let b e (BindState fun' arg)
            | Let b e arg' <- arg -> Just $ Let b e (BindState fun arg')
        _ -> Nothing

-- -- | Reduce lambdas by simulating application
-- -- Aka `(\a -> E) N` -> `let a = N in E`
-- -- Assumes lambda refs have been inlined
-- inlineLambda :: Expression -> Maybe Expression
-- inlineLambda = \case
--     Apply (Lambda b e) arg -> Just $ Let b arg e
--     _ -> Nothing
-- -- | Inline all direct reassignments.
-- -- Aka `let x = E in let y = x in y` -> `let x = E in x`
-- inlineReassignments :: Expression -> Expression
-- inlineReassignments = flip runReader HM.empty . cata go
--   where
--     go (LetF bnd val body) =
--         val >>= \case
--             v@(Var _) -> local (HM.insert bnd v) body
--             v -> Let bnd v <$> body
--     go (VarF val) = asks (fromMaybe (Var val) . HM.lookup val)
--     go e = embed <$> sequence e
-- | Transforms the final expression into a let expression with the result variable as body.
-- Aka `let x = E in some/sf a` -> `let x = E in let y = some/sf a in y`
--
-- EDIT: Now also does the same for any residual lambdas
ensureFinalLet :: MonadOhua m => Expression -> m Expression
ensureFinalLet = ensureFinalLetInLambdas >=> ensureFinalLet'

-- | Transforms the final expression into a let expression with the result variable as body.
ensureFinalLet' :: MonadOhua m => Expression -> m Expression
ensureFinalLet' (Let a e b) = Let a e <$> ensureFinalLet' b
ensureFinalLet' v@(Var _) = return v
ensureFinalLet' a = do
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
        val' <-
            val >>= \case
                v@(Var bnd') ->
                    asks (HM.lookup bnd') >>=
                    maybe (pure v) (\e' -> tell (HS.singleton bnd') >> pure e')
                other0 -> pure other0
        (body', touched) <- listen $ local (HM.insert bnd val') body
        pure $
            if bnd `HS.member` touched
                then body'
                else Let bnd val' body'
    inlinePartials (ApplyF (Var bnd, _) (_, arg)) = do
        tell $ HS.singleton bnd
        val <- asks (HM.lookup bnd)
        Apply <$>
            (maybe
                 (failWith $ "No suitable value found for binding " <> show bnd)
                 pure
                 val) <*>
            arg
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

-- -- | Lifts something like @if (f x) a b@ to @let x0 = f x in if x0 a b@
-- liftApplyToApply :: MonadOhua m => Expression -> m Expression
-- liftApplyToApply =
--     lrPrewalkExprM $ \case
--         Apply fn arg@(Apply _ _) -> do
--             bnd <- generateBinding
--             return $ Let bnd arg $ Apply fn (Var bnd)
--         a -> return a
-- normalizeBind :: (MonadError Error m, MonadGenBnd m) => Expression -> m Expression
-- normalizeBind =
--     rewriteM $ \case
--         BindState e1@(PureFunction _ _) e2 ->
--             case e2 of
--                 Var _ -> pure Nothing
--                 Lit _ -> pure Nothing
--                 _ ->
--                     generateBinding >>= \b ->
--                         pure $ Just $ Let b e2 (BindState e1 (Var b))
--         BindState _ _ -> throwError "State bind target must be a pure function reference"
--         _ -> pure Nothing
-- -- The canonical composition of the above transformations to create a
-- -- program with the invariants we expect.
-- normalize :: (MonadPlus m, MonadOhua m) => Expression -> m Expression
-- normalize e =
--     reduceLambdas e >>= removeCurrying >>= liftApplyToApply >>=
--     ensureFinalLet . inlineReassignments . letLift >>=
--     normalizeBind >>=
--     ensureAtLeastOneCall
--     -- we repeat this step until a fix point is reached.
--     -- this is necessary as lambdas may be input to lambdas,
--     -- which means after inlining them we may be able again to
--     -- inline a ref and then inline the lambda.
--     -- I doubt this will ever do more than two or three iterations,
--     -- but to make sure it accepts every valid program this is necessary.
--   where
--     letLift = rewrite floatOutLet
--     reduceLambdas =
--         evaluatingStateT mempty .
--         rewriteM
--             (\e ->
--                  pure (floatOutLet e) <|> pure (inlineLambda e) <|>
--                  inlineLambdaRefs e)
-- | A new version of 'normalize' which makes individual transformations simpler
-- by specifying them as small "single term rewrites" which are combined and
-- applied using 'rewriteM'
normalize ::
       forall m. (MonadOhua m)
    => Expression
    -> m Expression
normalize =
    ensureAtLeastOneCall <=<
    ensureFinalLet <=<
    removeCurrying <=<
    rewriteM
        (fmap asum .
         sequenceA .
         (sequenceA
              [ pure . floatOutLet
              , liftArguments
              , pure . inlinings
              , pure . inlineLambda
              ] :: Expr -> [m (Maybe Expr)]))
  where
    liftArguments =
        \case
            Apply fn arg@Apply {} -> do
                b <- generateBinding
                pure $ Just $ Let b arg (Apply fn (Var b))
            BindState fn arg
                | Lit {} <- arg -> pure Nothing
                | Var {} <- arg -> pure Nothing
                | otherwise -> do
                    b <- generateBinding
                    pure $ Just $ Let b arg $ Apply fn $ Var b
            _ -> pure Nothing

inlineLambda =
    \case
        Apply (Lambda v b) arg -> Just $ Let v arg b
        _ -> Nothing

-- | Inline stuff
inlinings =
    \case
        Let v e body
            | Lambda {} <- e -> issueReplace -- inline lambda refs
            | Var {} <- e -> issueReplace -- inline variable reassignments
            | BindState {} <- e -> issueReplace -- inline the state bindings they
                                              -- propagate inside applications
            | Lit {} <- e -> issueReplace -- I assume we can safely duplicate literals
            where issueReplace =
                      Just $
                      rewrite
                          (\case
                               Var v'
                                   | v == v' -> Just e
                               _ -> Nothing)
                          body
        _ -> Nothing
