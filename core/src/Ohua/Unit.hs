module Ohua.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.DFLang.Lang


cleanUnits :: Applicative m => DFExpr -> m DFExpr
cleanUnits (DFExpr lets ret) = pure $ DFExpr (fmap f lets) ret
  where
    f e@(LetExpr{callArguments=[a]}) | a == dfVarUnit = e {callArguments = []}
    f e                              = e

unitSym :: Lit
unitSym = UnitLit


unitExpr :: Expr
unitExpr = Lit unitSym

someUnitExpr :: Expr
someUnitExpr = unitExpr

dfVarUnit :: DFVar
dfVarUnit = DFEnvVar UnitLit
