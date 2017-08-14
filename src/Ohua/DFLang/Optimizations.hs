module Ohua.DFLang.Optimizations where


import Ohua.Monad
import Control.Monad.Except
import Ohua.DFLang.Lang
import Ohua.DFLang.Passes


runOptimizations :: (MonadError String m, MonadOhua m) => DFExpr -> m DFExpr
runOptimizations = return
