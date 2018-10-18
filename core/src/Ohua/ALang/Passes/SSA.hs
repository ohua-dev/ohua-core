-- |
-- Module      : $Header$
-- Description : Transform an algorithm language term into single static assignment form
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Ohua.ALang.Passes.SSA where

import Ohua.Prelude

import Data.Functor.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Ohua.ALang.Lang

type LocalScope = HM.HashMap Binding Binding

ssaResolve :: MonadReader LocalScope m => Binding -> m Binding
ssaResolve bnd = reader $ fromMaybe bnd <$> HM.lookup bnd

-- | Generate a new name for the provided binding and run the inner
-- computation with that name in scope to replace the provided binding
-- Returns both the generated binding and the result of the inner
-- computation
--
-- Passing in the computation which is to be executed in the modified
-- environment makes this function a bit harder to use (or rather the
-- code using it less readable) because it does a lot of passing
-- functions as arguments, however it very nicely encapsulates the
-- scope changes which means they will never leak from where they are
-- supposed to be applied
ssaRename ::
       (MonadGenBnd m, MonadReader LocalScope m)
    => Binding
    -> (Binding -> m a)
    -> m a
ssaRename oldBnd cont = do
    newBnd <- generateBindingWith oldBnd
    local (HM.insert oldBnd newBnd) $ cont newBnd

ssaRenameMany ::
       (MonadGenBnd m, MonadReader LocalScope m)
    => [Binding]
    -> ([Binding] -> m a)
    -> m a
ssaRenameMany oldBnds cont = do
    newBnds <- traverse generateBindingWith oldBnds
    local (HM.union $ HM.fromList $ zip oldBnds newBnds) $ cont newBnds

performSSA :: MonadOhua envExpr m => Expression -> m Expression
performSSA = flip runReaderT mempty . ssa

flattenTuple :: (a, ([a], b)) -> ([a], b)
flattenTuple (a, (as, r)) = (a : as, r)

ssa :: (MonadOhua envExpr m, MonadReader LocalScope m)
    => Expression
    -> m Expression
ssa =
    cata $ \case
        VarF (Local bnd) -> Var . Local <$> ssaResolve bnd
        LambdaF args body ->
            handleAssignment args $ \assign -> Lambda assign <$> body
        LetF assignment val body ->
            handleAssignment assignment $ \assign ->
                Let assign <$> val <*> body
        e -> embed <$> sequence e

-- As you can see the destructuring makes writing some stuff quite
-- difficult.  I wonder if it might not be easier to represent
-- destructuring with a builtin function instead and collapse it down
-- at the very end ...
handleAssignment ::
       (MonadOhua envExpr m, MonadReader LocalScope m)
    => Assignment
    -> (Assignment -> m t)
    -> m t
handleAssignment (Direct d) = ssaRename d . (. Direct)
handleAssignment (Recursive d) = ssaRename d . (. Recursive)
handleAssignment (Destructure ds) = ssaRenameMany ds . (. Destructure)

-- Check if an expression is in ssa form. Returns @Nothing@ if it is
-- SSA Returns @Just aBinding@ where @aBinding@ is a binding which was
-- defined (at least) twice
isSSA :: Expression -> Maybe Binding
isSSA =
    either Just (const Nothing) . flip evalState mempty . runExceptT . cata go
  where
    failOrInsert bnd = do
        isDefined <- gets (HS.member bnd)
        when isDefined $ throwErrorDebugS bnd
        modify (HS.insert bnd)
    go e = do
        case e of
            LetF assign _ _ -> mapM_ failOrInsert $ extractBindings assign
            LambdaF assign _ -> mapM_ failOrInsert $ extractBindings assign
            _ -> pure ()
        sequence_ e

checkSSA :: MonadOhua envExpr m => Expression -> m ()
checkSSA = maybe (return ()) (throwErrorDebugS . mkMsg) . isSSA
  where
    mkMsg bnd = "Redefinition of binding " <> show bnd
