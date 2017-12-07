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
import           Data.Monoid
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs     as Refs
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util
import qualified Ohua.Util.Str as Str

data IfFn = IfFn
    { conditionVariable :: !DFVar
    , thenBranch        :: !Lambda
    , elseBranch        :: !Lambda
    , ifRet             :: !Binding
    }


instance HigherOrderFunction IfFn where
    name = "ohua.lang/if"

    parseCallAndInitState
        [ Variable v
        , LamArg thenBr@(Lam{beginAssignment=Direct ret})
        , LamArg elseBr
        ] = pure $ IfFn v thenBr elseBr ret
    parseCallAndInitState args = failWith $ "Unexpected number or type of argument for if " <> Str.showS args

    createContextEntry = do
        f <- get
        ifId <- generateId
        let Direct thenBnd = beginAssignment $ thenBranch f
            Direct elseBnd = beginAssignment $ elseBranch f
        modify $ \s -> s { ifRet = thenBnd }
        pure [ LetExpr ifId (Destructure [thenBnd, elseBnd]) Refs.ifThenElse [conditionVariable f] Nothing ]

    createContextExit assignment = do
        switchId <- generateId
        IfFn {..} <- get
        pure
            [ LetExpr switchId assignment Refs.switch [DFVar ifRet, DFVar $ resultBinding thenBranch, DFVar $ resultBinding elseBranch] Nothing
            ]

    scopeFreeVariables lam freeVars = do
        selected <- mapM generateBindingWith freeVars
        selectorId <- generateId
        let Direct sourceVar = beginAssignment lam
        pure
            (   [ LetExpr selectorId (Destructure selected) Refs.scope (map DFVar freeVars) (Just sourceVar)
                ]
            ,   zip freeVars selected
            )

    contextifyUnboundFunctions (Lam (Direct x) _) = return $ Just ([], x)
    contextifyUnboundFunctions _ = failWith "Unexpected destructuring in begin assignment"
