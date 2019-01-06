module Ohua.ALang.Passes.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang

mkUnitFunctionsExplicit :: Expression -> Expression
mkUnitFunctionsExplicit e =
    flip transform e $ \case
        Let v (Apply f@(Lit (FunRefLit _)) u@(Lit UnitLit)) ie ->
            Let
                v
                -- FIXME the -1 is a hack because serialization otherwise prints null.
                --       it feels to me that the id should be part of the outermost Lit
                --       instead of the FunRef
                ((Lit $ FunRefLit $ FunRef "lang/unitFn" $ Just (-1)) `Apply` f `Apply`
                 u)
                ie
        other -> other
