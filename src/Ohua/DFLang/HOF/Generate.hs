module Ohua.DFLang.HOF.Generate (GenFn) where

import Ohua.Prelude

import qualified Ohua.ALang.Refs as ARefs
import qualified Ohua.Constants.HostExpr as HEConst
import Ohua.DFLang.HOF
import Ohua.DFLang.HOF.SmapG
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs


data GenFn = GenFn
  { lam :: Lambda
  , pulse :: Binding
  }


instance HigherOrderFunction GenFn where
    hofName = tagFnName ARefs.generate
    parseCallAndInitState [LamArg g] = pure $ GenFn g $ error "Pulse binding is uninitialized"
    parseCallAndInitState _ =
        failWith "Wrong number/type of argument to generate"
    createContextEntry = do
        GenFn {lam} <- get
        idId <- generateId
        mergeId <- generateId
        isJustId <- generateId
        initBnd <- generateBindingWith "init"
        resultTriggerBnd <- generateBindingWith "resultTrigger"
        pulseBnd <- generateBindingWith "pulse"
        modify $ \s -> s {pulse = pulseBnd}
        pure
            [ LetExpr idId (Direct initBnd) Refs.id [DFEnvVar HEConst.true] Nothing
            , LetExpr
                  isJustId
                  (Direct resultTriggerBnd)
                  Refs.isJust
                  [DFVar $ resultBinding lam]
                  Nothing
            , LetExpr
                  mergeId
                  (Direct pulseBnd)
                  Refs.ndMerge
                  [DFVar initBnd, DFVar resultTriggerBnd]
                  Nothing
            ]
    createContextExit (Direct b) = do
        GenFn {lam} <- get
        toGenId <- generateId
        pure
            [ LetExpr
                  toGenId
                  (Direct b)
                  Refs.toGen
                  [DFVar $ resultBinding lam]
                  Nothing
            ]
    createContextExit a =
        throwError $
        "generator assignment must be to one direct binding: " <> show a
    scopeFreeVariables _ freeVars = do
        GenFn {pulse} <- get
        (scopeExpr, varMap) <- scopeVars pulse freeVars
        pure ([scopeExpr], varMap)
    contextifyUnboundFunctions _ = do
        GenFn {pulse} <- get
        pure $ Just ([], pulse)
