-- |
-- Module      : $Header$
-- Description : Passes over algorithm language terms to ensure certain invariants
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.ALang.Passes where


import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.RWS.Lazy
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Maybe             (fromMaybe)
import           Ohua.ALang.Lang
import           Ohua.ALang.Util
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util.Str          as Str



-- | Inline all references to lambdas.
-- Aka `let f = (\a -> E) in f N` -> `(\a -> E) N`
inlineLambdaRefs :: MonadOhua envExpr m => Expression -> m Expression
inlineLambdaRefs = flip runReaderT mempty . para go
  where
    go (LetF assignment (Lambda _ _,l) (_, body)) =
      case assignment of
        Direct bnd  -> l >>= \l' -> local (HM.insert bnd l') body
        Recursive _ -> Let assignment <$> l <*> body
        Destructure _ -> failWith "invariant broken, cannot destructure lambda"
    go (VarF val@(Local bnd)) = asks (fromMaybe (Var val) .  HM.lookup bnd)
    go e = embed <$> traverse snd e


-- | Reduce lambdas by simulating application
-- Aka `(\a -> E) N` -> `let a = N in E`
-- Assumes lambda refs have been inlined
inlineLambda :: Expression -> Expression
inlineLambda = cata $ \case
  ApplyF (Lambda assignment body) argument -> Let assignment argument body
  ApplyF v@(Apply _ _) argument -> reduceLetCWith f v
    where
      f (Lambda assignment body) = Let assignment argument body
      f v0                       = Apply v0 argument
  e -> embed e

-- recursively performs the substitution
--
-- let x = (let y = M in A) in E[x] -> let y = M in let x = A in E[x]
reduceLetA :: Expression -> Expression
reduceLetA = \case
  Let assign (Let assign2 val expr3) expr -> Let assign2 val $ reduceLetA $ Let assign expr3 expr
  e -> e

reduceLetCWith :: (Expression -> Expression) -> Expression -> Expression
reduceLetCWith f = \case
  Apply (Let assign val expr) argument -> Let assign val $ reduceLetCWith f $ Apply expr argument
  e -> f e

reduceLetC :: Expression -> Expression
reduceLetC = reduceLetCWith id

reduceAppArgument :: Expression -> Expression
reduceAppArgument = \case
  Apply function (Let assign val expr) -> Let assign val $ reduceApplication $ Apply function expr
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
letLift = cata $ \e ->
  let f = case e of
            e@(LetF _ _ _) -> reduceLetA
            e@(ApplyF _ _) -> reduceApplication
            _              -> id
  in f $ embed e


idName :: QualifiedBinding
idName = QualifiedBinding (nsRefFromList ["ohua", "lang"]) "id"

-- | Inline all direct reassignments.
-- Aka `let x = E in let y = x in y` -> `let x = E in x`
inlineReassignments :: Expression -> Expression
inlineReassignments = flip runReader mempty . cata go
  where
    go (LetF assign val body) = val >>= \case
      v@(Var _) -> case assign of
                     Direct bnd -> local (HM.insert bnd v) body
                     Destructure _ -> Let assign (Apply (Var (Sf idName Nothing)) v) <$> body
                     Recursive _ -> error "TODO implement inlining reassignments for recursive bindings"
      v -> Let assign v <$> body
    go (VarF val@(Local bnd)) = asks (fromMaybe (Var val) . HM.lookup bnd)
    go e = embed <$> sequence e


-- | Transforms the final expression into a let expression with the result variable as body.
-- Aka `let x = E in some/sf a` -> `let x = E in let y = some/sf a in y`
--
-- EDIT: Now also does the same for any residual lambdas
ensureFinalLet :: MonadOhua envExpr m => Expression -> m Expression
ensureFinalLet = ensureFinalLetInLambdas >=> ensureFinalLet'


-- | Transforms the final expression into a let expression with the result variable as body.
ensureFinalLet' :: MonadOhua envExpr m => Expression -> m Expression
ensureFinalLet' (Let a e b) = Let a e <$> ensureFinalLet' b
ensureFinalLet' v@(Var _) = return v
ensureFinalLet' a = do
    newBnd <- generateBinding
    return $ Let (Direct newBnd) a (Var (Local newBnd))


ensureFinalLetInLambdas :: MonadOhua envExpr m => Expression -> m Expression
ensureFinalLetInLambdas = cata $ \case
    LambdaF bnd body -> Lambda bnd <$> (ensureFinalLet' =<< body)
    a -> embed <$> sequence a


ensureAtLeastOneCall :: (Monad m, MonadGenBnd m) => Expression -> m Expression
ensureAtLeastOneCall e@(Var _) = do
  newBnd <- generateBinding
  pure $ Let (Direct newBnd) (Var (Sf idName Nothing) `Apply` e) $ Var (Local newBnd)
ensureAtLeastOneCall e = cata f e
  where
    f expr@(LambdaF bnd body) = body >>= \case
      v@(Var _) -> do
        newBnd <- generateBinding
        pure $ Lambda bnd $ Let (Direct newBnd) (Var (Sf idName Nothing) `Apply` v) $ Var (Local newBnd)
      e -> pure $ Lambda bnd e
    f e = embed <$> sequence e


-- | Removes bindings that are never used.
-- This is actually not safe becuase sfn invocations may have side effects
-- and therefore cannot be removed.
-- Assumes ssa for simplicity
removeUnusedBindings :: Expression -> Expression
removeUnusedBindings = fst . runWriter . cata go
  where
    go (VarF val@(Local b)) = tell (HS.singleton b) >> return (Var val)
    go (LetF bnds val body) = do
        (inner, used) <- listen body
        if not $ any (`HS.member` used) $ flattenAssign bnds
          then
            return inner
          else do
            val' <- val
            pure $ Let bnds val' inner
    go e = embed <$> sequence e


-- | Reduce curried expressions.
-- aka `let f = some/sf a in f b` becomes `some/sf a b`.
-- It both inlines the curried function and removes the binding site.
-- Recursively calls it self and therefore handles redefinitions as well.
-- It only substitutes vars in the function positions of apply's
-- hence it may produce an expression with undefined local bindings.
-- It is recommended therefore to check this with 'noUndefinedBindings'.
-- If an undefined binding is left behind which indicates the source expression
-- was not fulfilling all its invariants.
removeCurrying :: forall m. MonadError Error m => Expression -> m Expression
removeCurrying e = fst <$> evalRWST (para inlinePartials e) mempty ()
  where
    inlinePartials (LetF assign@(Direct bnd) (_, val) (_, body)) = do
        val' <- val
        (body', touched) <- listen $ local (HM.insert bnd val') body
        pure $
            if bnd `HS.member` touched then
                body'
            else
                Let assign val' body'
    inlinePartials (ApplyF (Var (Local bnd), _) (_, arg)) = do
        tell $ HS.singleton bnd
        val <- asks (HM.lookup bnd)
        Apply
          <$> (maybe (failWith $ "No suitable value found for binding " <> Str.showS bnd) pure val)
          <*> arg
    inlinePartials e = embed <$> traverse snd e


-- | Ensures the expression is a sequence of let statements terminated with a local variable.
hasFinalLet :: MonadOhua envExpr m => Expression -> m ()
hasFinalLet (Let _ _ body)  = hasFinalLet body
hasFinalLet (Var (Local _)) = return ()
hasFinalLet (Var _)         = failWith "Non-local final var"
hasFinalLet _               = failWith "Final value is not a var"


-- | Ensures all of the optionally provided stateful function ids are unique.
noDuplicateIds :: MonadError Error m => Expression -> m ()
noDuplicateIds = flip evalStateT mempty . cata go
  where
    go e@(VarF (Sf _ (Just funid))) = do
      isMember <- gets (HS.member funid)
      when isMember $ failWith $ "Duplicate id " <> Str.showS funid
      modify (HS.insert funid)
    go e = sequence_ e


-- | Checks that no apply to a local variable is performed.
-- This is a simple check and it will pass on complex expressions even if they would reduce
-- to an apply to a local variable.
applyToSf :: MonadOhua envExpr m => Expression -> m ()
applyToSf = para $ \case
  ApplyF (Var (Local bnd),_) _ -> failWith $ "Illegal Apply to local var " <> Str.showS bnd
  e -> sequence_ $ fmap snd e

-- FIXME this function is never called. was it supposed to be part of the below validity check?
lamdasAreInputToHigherOrderFunctions :: MonadOhua envExpr m => Expression -> m ()
lamdasAreInputToHigherOrderFunctions _                         = return ()
-- lamdasAreInputToHigherOrderFunctions (Apply v (Lambda _ body)) = undefined


-- | Checks that all local bindings are defined before use.
-- Scoped. Aka bindings are only visible in their respective scopes.
-- Hence the expression does not need to be in SSA form.
noUndefinedBindings :: MonadOhua envExpr m => Expression -> m ()
noUndefinedBindings = flip runReaderT mempty . cata go
  where
    go (LetF (Recursive r) val body) = local (HS.insert r) $ val >> body
    go (LetF assign val body) = val >> local (HS.union $ HS.fromList $ flattenAssign assign) body
    go (VarF (Local bnd)) = do
        isDefined <- asks (HS.member bnd)
        unless isDefined $ failWith $ "Not in scope " <> Str.showS bnd
    go (LambdaF assign body) = local (HS.union $ HS.fromList $ flattenAssign assign) body
    go e = sequence_ e


checkProgramValidity :: MonadOhua envExpr m => Expression -> m ()
checkProgramValidity e = do
    hasFinalLet e
    noDuplicateIds e
    applyToSf e
    noUndefinedBindings e


-- | Lifts something like @if (f x) a b@ to @let x0 = f x in if x0 a b@
liftApplyToApply :: MonadOhua envExpr m => Expression -> m Expression
liftApplyToApply = lrPrewalkExprM $ \case
  Apply fn arg@(Apply _ _) -> do
    bnd <- generateBinding
    return $ Let (Direct bnd) arg $ Apply fn (Var (Local bnd))
  a -> return a


-- The canonical composition of the above transformations to create a program with the invariants we expect.
normalize :: MonadOhua envExpr m => Expression -> m Expression
normalize e =
        reduceLambdas (letLift e)
    >>= removeCurrying
    >>= liftApplyToApply
    >>= ensureFinalLet . inlineReassignments . letLift
    >>= ensureAtLeastOneCall
  where
    -- we repeat this step until a fix point is reached.
    -- this is necessary as lambdas may be input to lambdas,
    -- which means after inlining them we may ba able again to
    -- inline a ref and then inline the lambda.
    -- I doubt this will ever do more than two or three iterations,
    -- but to make sure it accepts every valid program this is necessary.
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
--         v'@()


type EnvOnlyExpr = Expr (Either Binding HostExpr)


{-|
Find complete subtrees in ALang which are only dependent on environemt values.
Uses a user supplied function which is supposed to turn this inte some constant
environment expression.

The idea is that you can use this to create a custom pass by supplying a
domain specific compression function.
-}
compressEnvExpressions :: forall env m. MonadOhua env m
                       => (EnvOnlyExpr -> m env) -> Expression -> m Expression
compressEnvExpressions compress = either pure compress' <=< go
  where
    go :: Expression -> m (Either Expression EnvOnlyExpr)
    go (Var v) = pure $
        case v of
            Env e   -> Right $ Var $ Right e
            Local l -> Right $ Var $ Left l
            other   -> Left $ Var other
    go (Apply e1 e2) = do
        e1' <- go e1
        e2' <- go e2
        case liftA2 Apply e1' e2' of
            Right a -> pure $ Right a
            Left _  -> Left <$> liftA2 Apply (mcompress e1') (mcompress e2')
    go (Let assign val body) = do
        val' <- go val
        body' <- go body
        case liftA2 (Let assign) val' body' of
            Right a -> pure $ Right a
            Left _ -> Left <$> liftA2 (Let assign) (mcompress val') (mcompress body')
    go (Lambda assign body) = bimap (Lambda assign) (Lambda assign) <$> go body

    mcompress :: Either Expression EnvOnlyExpr -> m Expression
    mcompress = either pure compress'
    compress' expr = do
        compressed <- compress expr
        ref <- addEnvExpression compressed
        pure $ Var $ Env ref
