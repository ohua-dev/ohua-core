module Ohua.ALang.Passes.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang

mkUnitFunctionsExplicit :: Expression -> Expression
mkUnitFunctionsExplicit e =
    flip transform e $ \case
        Let v (Apply f@(Lit (FunRefLit _)) u@(Lit UnitLit)) ie ->
            Let
                v
                ((Lit $ FunRefLit $ FunRef "lang/unitFn" Nothing) `Apply` f `Apply`
                 u)
                ie
        other -> other
