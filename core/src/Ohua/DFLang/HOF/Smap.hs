-- |
-- Module      : $Header$
-- Description : Implementation for the @smap@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}

#include "compat.h"

module Ohua.DFLang.HOF.Smap where

import Ohua.Prelude

import qualified Data.Sequence as S

import qualified Ohua.ALang.Refs as ARefs
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs


data SmapFn = SmapFn
    { collSource :: !DFVar
    , smapLambda :: !Lambda
    , sizeSource :: Binding
    }

instance HigherOrderFunction SmapFn where
    hofName = tagFnName ARefs.smap
    parseCallAndInitState [LamArg lam, Variable v] =
        return $ SmapFn v lam (error "size uninitialized")
    parseCallAndInitState a =
        throwErrorDebugS $ "Unexpected number/type of arguments to smap" <> show a
    createContextEntry = do
        f <- get
        sizeId <- generateId
        sizeRet <- generateBindingWith "size"
        otnId <- generateId
        otnRet <- generateBinding
        smapId <- generateId
        modify $ \s -> s {sizeSource = sizeRet} -- do we need the size or otn of the size?
        return
            [ LetExpr sizeId (Direct sizeRet) Refs.size [collSource f] Nothing
            -- I am not sure about the order here ... is it size first or collection first?
            , LetExpr
                  otnId
                  (Direct otnRet)
                  Refs.oneToN
                  [DFVar sizeRet, collSource f]
                  Nothing
            , LetExpr
                  smapId
                  (beginAssignment $ smapLambda f)
                  Refs.smapFun
                  [DFVar otnRet]
                  Nothing
            ]
    createContextExit (Destructure d) =
        throwErrorDebugS $ "Smap result cannot be destructured: " <> show d
    createContextExit assignment = do
        collectId <- generateId
        f <- get
        otnId <- generateId
        repeatedSize <- generateBinding
        let sizeVar = DFVar $ sizeSource f
        return
            [ LetExpr
                  otnId
                  (Direct repeatedSize)
                  Refs.oneToN
                  [sizeVar, sizeVar]
                  Nothing
            , LetExpr
                  collectId
                  assignment
                  Refs.collect
                  [DFVar $ repeatedSize, DFVar $ resultBinding $ smapLambda f]
                  Nothing
            ]
    scopeFreeVariables _ freeVars = do
        SmapFn {sizeSource} <- get
        let mkReplicator var = do
                funid <- generateId
                newVar <- generateBindingWith var
                pure
                    ( LetExpr
                          funid
                          (Direct newVar)
                          Refs.oneToN
                          [DFVar sizeSource, DFVar var]
                          Nothing
                    , newVar)
        (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
        return (S.fromList replicators, zip freeVars replications)
    contextifyUnboundFunctions lam = do
        ctxSource <- generateBindingWith "_smap_context"
        ctxOp <-
            do ctxId <- generateId
               pure $
                   LetExpr
                       ctxId
                       (Direct ctxSource)
                       Refs.seq
                       [DFVar elemSource]
                       Nothing
        pure $ Just ([ctxOp], ctxSource)
      where
        elemSource =
            fromMaybe (error "Begin assignment should have one argument") $ safeHead $
            extractBindings $ beginAssignment lam