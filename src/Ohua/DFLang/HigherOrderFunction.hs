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
module Ohua.DFLang.HigherOrderFunction where


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


data Lambda = Lam
    { beginAssignment :: Assignment
    , resultBinding   :: Binding
    } deriving Eq


data Argument
    = Variable DFVar
    | LamArg Lambda


tagFnName :: FnName -> TaggedFnName f
tagFnName = TaggedFnName


type Renaming = [(Binding, Binding)]

class HigherOrderFunction f where

    name :: TaggedFnName f

    init :: (MonadOhua m, MonadError String m) => [Argument] -> m f

    begin :: (MonadOhua m, MonadError String m, MonadState f m) => m (Seq LetExpr)

    end :: (MonadOhua m, MonadError String m, MonadState f m) => Assignment -> m (Seq LetExpr)

    scopeFreeVariables :: (MonadOhua m, MonadError String m, MonadState f m) => Lambda -> [Binding] -> m (Seq LetExpr, Renaming)

    scopeUnboundFunctions :: (MonadOhua m, MonadError String m, MonadState f m) => Lambda -> m Bool


data WHOF = forall f . HigherOrderFunction f => WHOF (Proxy f)

