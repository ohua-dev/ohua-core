module Ohua.Unit where


import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang


cleanUnits :: Applicative m => DFExpr -> m DFExpr
cleanUnits (DFExpr lets ret) = pure $ DFExpr (fmap f lets) ret
  where
    f e@(LetExpr{callArguments=[a]}) | a == dfVarUnit = e {callArguments = []}
    f e                              = e


unitHE :: HostExpr
unitHE = HostExpr (-1)


unitSym :: ResolvedSymbol
unitSym = Env unitHE


unitExpr :: AExpr s ResolvedSymbol
unitExpr = Var unitSym


dfVarUnit :: DFVar
dfVarUnit = DFEnvVar unitHE
