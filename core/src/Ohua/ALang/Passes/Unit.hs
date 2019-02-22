module Ohua.ALang.Passes.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang

mkUnitFunctionsExplicit :: Expression -> Expression
mkUnitFunctionsExplicit e =
    flip transform e $ \case
        Let v (Apply (Lit (FunRefLit (FunRef f _))) u@(Lit UnitLit)) ie ->
            Let
                v
                ((Lit $ FunRefLit $ FunRef "ohua.lang/unitFn" Nothing) `Apply`
                 (Lit $ FunRefLit $ FunRef f Nothing) `Apply`
                 u)
                ie
        other -> other
