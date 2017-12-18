-- |
-- Module      : $Header$
-- Description : Implementation for the @seq@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedLists #-}
module Ohua.DFLang.HOF.Seq where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Sequence        as S
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types
import qualified Ohua.DFLang.Refs as Refs

data SeqFn = SeqFn {
  before    :: Binding,
  afterExpr :: Lambda
}

instance HigherOrderFunction SeqFn where
  name = "ohua.lang/seq"

  parseCallAndInitState [Variable (DFVar before), LamArg after] = return $ SeqFn before after
  parseCallAndInitState as = failWith "seq not defined for arguments: " -- TODO ++ show as

  createContextEntry = return S.empty

  createContextExit assignment = return S.empty

  scopeFreeVariables lam freeVars = return (S.empty, [])

  contextifyUnboundFunctions _ = do
    seqExprOut <- generateBindingWith "seq"
    seqId <- generateId
    bndBefore <- gets before
    pure $ Just 
      ( [LetExpr seqId (Direct seqExprOut) Refs.seq [DFVar bndBefore] Nothing]
      , seqExprOut
      )
