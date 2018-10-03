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

unitSym :: ResolvedSymbol
unitSym = Env HEConst.unit


unitExpr :: AExpr s ResolvedSymbol
unitExpr = Var unitSym

unitBinding :: Binding
unitBinding = unsafeMake "()"

someUnitSymbol :: SomeBinding
someUnitSymbol = Unqual unitBinding

someUnitExpr :: AExpr s SomeBinding
someUnitExpr = Var someUnitSymbol

dfVarUnit :: DFVar
dfVarUnit = DFEnvVar HEConst.unit
