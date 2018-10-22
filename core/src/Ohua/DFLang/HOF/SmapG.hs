-- |
-- Module      : $Header$
-- Description : Implementation for the @smap-g@ higher order function.
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
--
-- This source code is licensed under the terms described in the associated LICENSE.TXT file

module Ohua.DFLang.HOF.SmapG (SmapGFn, scopeVars) where

import Ohua.Prelude

import qualified Ohua.ALang.Refs as ARefs
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs


data SmapGFn = SmapGFn
    { collSource :: !DFVar
    , smapLambda :: !Lambda
    , triggerArc :: Binding
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
              Refs.repeat
              (map DFVar $ scopeSource : freeVars)
              Nothing
        , zip freeVars newVars)


instance HigherOrderFunction SmapGFn where
    hofName = tagFnName ARefs.smapG
    parseCallAndInitState [LamArg lam, Variable v] =
        pure $ SmapGFn v lam $ error "Do not evaluate this"
    parseCallAndInitState a =
        throwErrorDebugS $ "Unexpected number/type of arguments to smap" <> show a
    createContextEntry = do
        SmapGFn {collSource, smapLambda} <- get
        smapId <- generateId
        items <- generateBindingWith "items"
        idId <- generateId
        triggerArcBnd <- generateBindingWith "trigger"
        modify $ \s -> s {triggerArc = triggerArcBnd}
        pure
            [ LetExpr
                  smapId
                  (Destructure [items, triggerArcBnd])
                  Refs.smapGFun
                  [collSource]
                  Nothing
            , LetExpr
                  idId
                  (beginAssignment smapLambda)
                  Refs.id
                  [DFVar items]
                  (Just triggerArcBnd)
            ]
    createContextExit (Destructure d) =
        throwErrorDebugS $ "Smap result cannot be destructured: " <> show d
    createContextExit assignment = do
        collectId <- generateId
        SmapGFn {triggerArc, smapLambda} <- get
        pure
            [ LetExpr
                  collectId
                  assignment
                  Refs.collectG
                  [DFVar triggerArc, DFVar $ resultBinding smapLambda]
                  Nothing
            ]
    scopeFreeVariables _ freeVars = do
        SmapGFn {triggerArc} <- get
        (scopeExpr, varMap) <- scopeVars triggerArc freeVars
        pure ([scopeExpr], varMap)
    contextifyUnboundFunctions _ = do
        SmapGFn {triggerArc} <- get
        pure $ Just (mempty, triggerArc)