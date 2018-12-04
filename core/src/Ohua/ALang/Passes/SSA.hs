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
import Control.Category ((>>>))
import Control.Lens (non, at)

import Ohua.ALang.Lang
import Ohua.ALang.Util

type LocalScope = HM.HashMap Binding Binding

ssaResolve :: MonadReader LocalScope m => Binding -> m Binding
ssaResolve bnd = view $ at bnd . non bnd

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

performSSA :: MonadOhua m => Expression -> m Expression
performSSA = flip runReaderT mempty . ssa

ssa :: (MonadOhua m, MonadReader LocalScope m)
    => Expression
    -> m Expression
ssa =
    cata $ \case
        VarF bnd -> Var <$> ssaResolve bnd
        LambdaF v body -> ssaRename v $ \v' -> Lambda v' <$> body
        LetF v val body -> ssaRename v $ \v' -> Let v' <$> val <*> body
        e -> embed <$> sequence e

-- Check if an expression is in ssa form. Returns @Nothing@ if it is
-- SSA Returns @Just aBinding@ where @aBinding@ is a binding which was
-- defined (at least) twice
isSSA :: Expression -> [Binding]
isSSA e = [b | (b, count) <- HM.toList counts, count > 1]
  where
    counts = HM.fromListWith (+) [(b, 1 :: Word) | b <- definedBindings e]



checkSSA :: MonadOhua m => Expression -> m ()
checkSSA = isSSA >>> \case
    [] -> return ()
    other -> throwErrorDebugS $ mkMsg other
  where
    mkMsg bnd = "Redefinition of bindings " <> show bnd
