-- |
-- Module      : $Header$
-- Description : Implementation for the @seq@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

module Ohua.DFLang.HOF.Seq where

import Protolude

import Data.Sequence as S

import qualified Ohua.ALang.Refs as ARefs
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.Monad
import Ohua.Types

data SeqFn = SeqFn
    { before :: Binding
    , afterExpr :: Lambda
    }

instance HigherOrderFunction SeqFn where
    name = tagFnName ARefs.seq
    parseCallAndInitState [Variable (DFVar before0), LamArg after] =
        return $ SeqFn before0 after
    parseCallAndInitState _ = failWith "seq not defined for arguments: " -- TODO ++ show arguments
    createContextEntry = return S.empty
    createContextExit _ = return S.empty
    scopeFreeVariables _ _ = return (S.empty, [])
    contextifyUnboundFunctions _ = do
        seqExprOut <- generateBindingWith "seq"
        seqId <- generateId
        bndBefore <- gets before
        pure $
            Just
                ( [ LetExpr
                        seqId
                        (Direct seqExprOut)
                        Refs.seq
                        [DFVar bndBefore]
                        Nothing
                  ]
                , seqExprOut)
