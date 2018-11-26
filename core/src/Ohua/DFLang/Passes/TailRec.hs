module Ohua.DFLang.Passes.TailRec where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Util

recurLowering :: DFExpr -> DFExpr
recurLowering (DFExpr letExprs returnVar) = undefined -- TODO How do I find the related calls of a recursion?
