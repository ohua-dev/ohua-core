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

module Ohua.DFLang.HOF.If
    ( IfFn
    ) where

import Ohua.Prelude

import qualified Ohua.ALang.Refs as ARefs
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs

data IfFn = IfFn
    { conditionVariable :: !DFVar
    , thenBranch :: !Lambda
    , elseBranch :: !Lambda
    , ifRet :: !Binding
    }

scopeVars ::
       (MonadGenBnd m, MonadGenId m, Monad m)
    => Binding
    -> [Binding]
    -> m (LetExpr, [(Binding, Binding)])
scopeVars scopeSource freeVars = do
    scopeId <- generateId
    newVars <- mapM generateBindingWith freeVars
    pure
        ( LetExpr
              scopeId
              (Destructure newVars)
              Refs.scope
              (map DFVar freeVars)
              (Just scopeSource)
        , zip freeVars newVars)

checkAssignment ::
       (Show a, MonadError Error m) => Text -> AbstractAssignment a -> m a
checkAssignment _ (Direct b) = pure b
checkAssignment branch otherBranch =
    throwErrorDebugS $
    "if HOF transform expects one direct assignment for `" <> branch <>
    "` branch, got " <>
    show otherBranch

instance HigherOrderFunction IfFn where
    hofName = tagFnName ARefs.ifThenElse
    parseCallAndInitState [Variable v, LamArg thenBr@(Lam {beginAssignment = Direct ret}), LamArg elseBr] =
        pure $ IfFn v thenBr elseBr ret
    parseCallAndInitState args =
        throwErrorDebugS $
        "Unexpected number or type of argument for if " <> show args
    createContextEntry = do
        f <- get
        ifId <- generateId
        thenBnd <- checkAssignment "then" (beginAssignment $ thenBranch f)
        elseBnd <- checkAssignment "else" (beginAssignment $ elseBranch f)
        modify $ \s -> s {ifRet = thenBnd}
        pure
            [ LetExpr
                  ifId
                  (Destructure [thenBnd, elseBnd])
                  Refs.bool
                  [conditionVariable f]
                  Nothing
            ]
    createContextExit assignment = do
        switchId <- generateId
        IfFn {..} <- get
        pure
            [ LetExpr
                  switchId
                  assignment
                  Refs.select
                  [ DFVar ifRet
                  , DFVar $ resultBinding thenBranch
                  , DFVar $ resultBinding elseBranch
                  ]
                  Nothing
            ]
    scopeFreeVariables lam freeVars = do
        sourceVar <-
            case beginAssignment lam of
                Direct v -> pure v
                _ ->
                    throwErrorDebugS $
                    "if HOF transform expects direct assignment in all branches"
        (scopeExpr, varMap) <- scopeVars sourceVar freeVars
        pure ([scopeExpr], varMap)
    contextifyUnboundFunctions (Lam (Direct x) _) = return $ Just ([], x)
    contextifyUnboundFunctions _ =
        failWith "Unexpected destructuring in begin assignment"
