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
{-# LANGUAGE ExistentialQuantification, CPP #-}
module Ohua.DFLang.HOF where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Proxy
import           Data.Sequence
import           Data.String
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types


newtype TaggedFnName f = TaggedFnName { unTagFnName :: FnName }

instance IsString (TaggedFnName a) where
    fromString = tagFnName . fromString

tagFnName :: FnName -> TaggedFnName f
tagFnName = TaggedFnName


data Lambda = Lam
    { beginAssignment :: Assignment
    , resultBinding   :: Binding
    } deriving Eq


data Argument
    = Variable DFVar
    | LamArg Lambda


type Renaming = [(Binding, Binding)]


-- mention method call order
class HigherOrderFunction f where
    name :: TaggedFnName f

    parseCallAndInitState :: (MonadOhua m, MonadError String m) => [Argument] -> m f

    createContextEntry :: (MonadOhua m, MonadError String m, MonadState f m) => m (Seq LetExpr)

    createContextExit :: (MonadOhua m, MonadError String m, MonadState f m) => Assignment -> m (Seq LetExpr)

    -- mention and impement compiler check for no free vars
    scopeFreeVariables :: (MonadOhua m, MonadError String m, MonadState f m) => Lambda -> [Binding] -> m (Seq LetExpr, Renaming)

    -- INVARIANT: unbound functions == functions with no arguments (see above)
    contextifyUnboundFunctions :: (MonadOhua m, MonadError String m, MonadState f m) => Lambda -> m Bool


data WHOF = forall f . HigherOrderFunction f => WHOF (Proxy f)

