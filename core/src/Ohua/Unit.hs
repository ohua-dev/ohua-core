module Ohua.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.Constants.HostExpr as HEConst
import Ohua.DFLang.Lang


cleanUnits :: Applicative m => DFExpr -> m DFExpr
cleanUnits (DFExpr lets ret) = pure $ DFExpr (fmap f lets) ret
  where
    f e@(LetExpr{callArguments=[a]}) | a == dfVarUnit = e {callArguments = []}
    f e                              = e

unitSym :: Lit
unitSym = UnitLit


unitExpr :: AExpr s
unitExpr = Lit unitSym

someUnitExpr :: AExpr s
someUnitExpr = unitExpr

dfVarUnit :: DFVar
dfVarUnit = DFEnvVar UnitLit
