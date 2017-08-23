module Ohua.DFLang.HOF.TailRec where

import Ohua.Types
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import Ohua.DFLang.Refs

data RecurFn = RecurFn
    {
        -- TODO
    }

-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur algoRef conditionOutput algoInArgs recurArgs).
instance HigherOrderFunction RecurFn where

    name = "ohua.lang/recur"

    parseCallAndInitState = undefined

    createContextEntry = undefined

    createContextExit assignment = undefined

    scopeFreeVariables lam freeVars = undefined

    contextifyUnboundFunctions lam = undefined
