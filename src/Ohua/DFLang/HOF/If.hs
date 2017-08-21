{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ohua.DFLang.HOF.If where

import           Control.Monad.Except
import           Control.Monad.State
import           Ohua.DFLang.HOF
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Ohua.Types

data IfFn = IfFn
    { conditionVariable :: !DFVar
    , thenBranch        :: !Lambda
    , elseBranch        :: !Lambda
    , ifRet             :: Binding
    }


instance HigherOrderFunction IfFn where
    name = "com.ohua.lang/if"

    parseCallAndInitState [Variable v, LamArg thenBr, LamArg elseBr] = return $ IfFn v thenBr elseBr (error "return uninitialized")
    parseCallAndInitState _ = throwError "Unexpected number or type of argument for if"

    createContextEntry = do
        f <- get
        ifId <- generateId
        ifRet' <- generateBinding
        let Direct thenBnd = beginAssignment $ thenBranch f
            Direct elseBnd = beginAssignment $ elseBranch f
        modify $ \s -> s { ifRet = ifRet' }
        return
            -- not sure this has to be a dffunction
            [ LetExpr ifId (Destructure [ifRet', thenBnd, elseBnd]) (DFFunction "com.ohua.lang/if") [conditionVariable f] Nothing
            ]

    createContextExit assignment = do
        switchId <- generateId
        IfFn {..} <- get
        return
            [ LetExpr switchId assignment (DFFunction "com.ohua.lang/switch") [DFVar ifRet, DFVar $ resultBinding thenBranch, DFVar $ resultBinding elseBranch] Nothing
            ]

    scopeFreeVariables lam freeVars = do
        selected <- mapM generateBindingWith freeVars
        selectorId <- generateId
        let Direct sourceVar = beginAssignment lam
        return   -- im just prepending the if return, this is probably not correct
            (   [ LetExpr selectorId (Destructure selected) (DFFunction "com.ohua.lang/scope") (map DFVar freeVars) (Just sourceVar)
                ]
            ,   zip freeVars selected
            )

    contextifyUnboundFunctions _ = return True

