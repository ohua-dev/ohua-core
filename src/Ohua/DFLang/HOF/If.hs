-- |
-- Module      : $Header$
-- Description : Implementation for the @if@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ohua.DFLang.HOF.If where


import           Control.Monad.Except
import           Control.Monad.State
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs     as Refs
import           Ohua.Monad
import           Ohua.Types


data IfFn = IfFn
    { conditionVariable :: !DFVar
    , thenBranch        :: !Lambda
    , elseBranch        :: !Lambda
    , ifRet             :: Binding
    }


instance HigherOrderFunction IfFn where
    name = "ohua.lang/if"

    parseCallAndInitState [Variable v, LamArg thenBr, LamArg elseBr] = return $ IfFn v thenBr elseBr (error "return uninitialized")
    parseCallAndInitState _ = failWith "Unexpected number or type of argument for if"

    createContextEntry = do
        f <- get
        ifId <- generateId
        ifRet' <- generateBinding
        let Direct thenBnd = beginAssignment $ thenBranch f
            Direct elseBnd = beginAssignment $ elseBranch f
        modify $ \s -> s { ifRet = ifRet' }
        return
            -- not sure this has to be a dffunction
            [ LetExpr ifId (Destructure [thenBnd, elseBnd]) Refs.ifThenElse [conditionVariable f] Nothing
            ]

    createContextExit assignment = do
        switchId <- generateId
        IfFn {..} <- get
        return
            [ LetExpr switchId assignment Refs.switch [DFVar ifRet, DFVar $ resultBinding thenBranch, DFVar $ resultBinding elseBranch] Nothing
            ]

    scopeFreeVariables lam freeVars = do
        selected <- mapM generateBindingWith freeVars
        selectorId <- generateId
        let Direct sourceVar = beginAssignment lam
        return   -- im just prepending the if return, this is probably not correct
            (   [ LetExpr selectorId (Destructure selected) Refs.scope (map DFVar freeVars) (Just sourceVar)
                ]
            ,   zip freeVars selected
            )

    contextifyUnboundFunctions (Lam (Direct x) _) = return $ Just x
    contextifyUnboundFunctions _ = failWith "Unexpected destructuring in begin assignment"

