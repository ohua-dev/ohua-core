module Ohua.DFLang.Passes.If where

import Ohua.Prelude

import Ohua.DFLang.Lang

-- | The `ctrl` on the ALang level is a function. The `ctrl` on the DFLang level
--   is an operator and as such we can remove the destructuring and do that
--   directly in the operator.
optimizeIf :: DFExpr -> DFExpr
optimizeIf = undefined -- TODO
