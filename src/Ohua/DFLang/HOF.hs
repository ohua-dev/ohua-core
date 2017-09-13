-- |
-- Module      : $Header$
-- Description : Abstract Higher Order Functions for Ohua
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
{-# LANGUAGE ExistentialQuantification #-}
module Ohua.DFLang.HOF where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Proxy
import           Data.Sequence
import           Data.String
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types


newtype TaggedFnName f = TaggedFnName { unTagFnName :: QualifiedBinding }

instance IsString (TaggedFnName a) where
    fromString = tagFnName . fromString

tagFnName :: QualifiedBinding -> TaggedFnName f
tagFnName = TaggedFnName


data Lambda = Lam
    { beginAssignment :: Assignment
    , resultBinding   :: Binding
    } deriving Eq


data Argument
    = Variable DFVar
    | LamArg Lambda


type Renaming = [(Binding, Binding)]


-- | Implementation of the lowering of a higher order function.
-- The type @f@ is dispatched based on the 'name'.
-- For each invocation of @name@ the lowering is executed once.
-- 'parseCallAndInitState' is called sepatately for each lowering
-- meaning there is no state sharing across invocations.
-- The call order for the methods of this class is fixed.
--
--      (1) 'parseCallAndInitState' is called with the arguments provided to the HOF call.
--      (2) 'createContextEntry' is called
--      (3) 'scopeFreeVariables' is called once for each lambda present in the arguments
--      (4) 'contextifyUnboundFunctions' is called once for each lambda present in the arguments
--      (5) 'createContextExit' is called
class HigherOrderFunction f where
    name :: TaggedFnName f

    -- | Initialize the state for a single lowering from the arguments given to the HOF call
    parseCallAndInitState :: MonadOhua m => [Argument] -> m f

    -- | Generate the entry node(s) for this HOF context
    createContextEntry :: (MonadOhua m, MonadState f m) => m (Seq LetExpr)

    -- | Generate the exit node(s) for this HOF context
    createContextExit :: (MonadOhua m, MonadState f m) => Assignment -> m (Seq LetExpr)

    -- | Create scope nodes for __all__ free variables of __one of the lambdas__ that were input to 'parseCallAndInitState'.
    -- This methiod is never called with a lambda which was not in the list given to 'parseCallAndInitState'.
    -- The compiler checks that this makes all previously free variables locally bound variables.
    scopeFreeVariables :: (MonadOhua m, MonadState f m) => Lambda -> [Binding] -> m (Seq LetExpr, Renaming)

    -- | Whether the compiler should add context args for all functions in this lambda
    -- which have no local variables as input.
    -- As an invariant, since 'scopeFreeVariables' makes all free varaibles local ones
    -- this should only apply to funcitons with no inputs at all.
    contextifyUnboundFunctions :: (MonadOhua m, MonadState f m) => Lambda -> m (Maybe Binding)


data WHOF = forall f . HigherOrderFunction f => WHOF (Proxy f)

