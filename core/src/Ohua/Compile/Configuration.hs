module Ohua.Compile.Configuration where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.DFLang.Lang

data CustomPasses env = CustomPasses
  { passBeforeNormalize  :: Expression -> OhuaM env Expression
  , passAfterNormalize  :: Expression -> OhuaM env Expression
  , passAfterDFLowering :: DFExpr -> OhuaM env DFExpr
  }

noCustomPasses :: CustomPasses env
noCustomPasses = CustomPasses pure pure pure

instance Default (CustomPasses env) where
  def = noCustomPasses
