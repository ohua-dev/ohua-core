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


import           Control.Monad.State
import           Data.Monoid
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs    as Refs
import           Ohua.Monad
import           Ohua.Types
import qualified Ohua.Util.Str       as Str

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
        let thenBnd = case beginAssignment $ thenBranch f of
                        Direct b -> b
                        _ -> error "if HOF transform expects one direct assignment for `then` branch"
            elseBnd = case beginAssignment $ elseBranch f of
                        Direct b -> b
                        _ -> error "if HOF transform expects one direct assignment for `else` branch"
        modify $ \s -> s { ifRet = thenBnd }
        pure [ LetExpr ifId (Destructure [thenBnd, elseBnd]) Refs.bool [conditionVariable f] Nothing ]

    createContextExit assignment = do
        switchId <- generateId
        IfFn {..} <- get
        pure
            [ LetExpr switchId assignment Refs.select [DFVar ifRet, DFVar $ resultBinding thenBranch, DFVar $ resultBinding elseBranch] Nothing
            ]

    scopeFreeVariables lam freeVars = do
        selected <- mapM generateBindingWith freeVars
        selectorId <- generateId
        let sourceVar = case beginAssignment lam of
                          Direct v -> v
                          _ -> error "if HOF transform expects direct assignment in all branches"
        pure
            (   [ LetExpr selectorId (Destructure selected) Refs.scope (map DFVar freeVars) (Just sourceVar)
                ]
            ,   zip freeVars selected
            )

    contextifyUnboundFunctions (Lam (Direct x) _) = return $ Just ([], x)
    contextifyUnboundFunctions _ = failWith "Unexpected destructuring in begin assignment"
