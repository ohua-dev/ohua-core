-- |
-- Module      : $Header$
-- Description : Implementation for the @smap@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
module Ohua.DFLang.HOF.Smap where


import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Sequence        as S
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types


data SmapFn = SmapFn
    { collSource :: !DFVar
    , smapLambda :: !Lambda
    , sizeSource :: Binding
    }


instance HigherOrderFunction SmapFn where
    name = "com.ohua.lang/smap"

    parseCallAndInitState [LamArg lam, Variable v] = return $ SmapFn v lam (error "size uninitialized")
    parseCallAndInitState _ = throwError "Unexpected number/type of arguments to smap"

    createContextEntry = do
        f <- get
        sizeId <- generateId
        sizeRet <- generateBindingWith "size"
        otnId <- generateId
        otnRet <- generateBinding
        smapId <- generateId
        modify $ \s -> s { sizeSource = otnRet } -- do we need the size or otn of the size?
        return
            [ LetExpr sizeId (Direct sizeRet) (EmbedSf "com.ohua.lang/size") [collSource f] Nothing
            -- I am not sure about the order here ... is it size first or collection first?
            , LetExpr otnId (Direct otnRet) (DFFunction "com.ohua.lang/one-to-n") [DFVar sizeRet, collSource f] Nothing
            , LetExpr smapId (beginAssignment $ smapLambda f) (DFFunction "com.ohua.lang/smap-fun") [DFVar otnRet] Nothing
            ]

    createContextExit assignment = do
        collectId <- generateId
        f <- get
        return
            [ LetExpr collectId assignment (DFFunction "com.ohua.lang/collect") [DFVar $ sizeSource f, DFVar $ resultBinding $ smapLambda f] Nothing
            ]

    scopeFreeVariables _ freeVars = do
        SmapFn{sizeSource} <- get
        let mkReplicator var = do
                id <- generateId
                newVar <- generateBindingWith var
                pure (LetExpr id (Direct newVar) (DFFunction "com.ohua.lang/one-to-n") [DFVar sizeSource, DFVar var] Nothing, newVar)
        (replicators, replications) <- unzip <$> mapM mkReplicator freeVars
        return (S.fromList replicators, zip freeVars replications)

    contextifyUnboundFunctions lam = return $ Just $ head $ flattenAssign $ beginAssignment lam
