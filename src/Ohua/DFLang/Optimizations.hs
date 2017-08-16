module Ohua.DFLang.Optimizations where


import           Control.Monad.Except
import           Ohua.DFLang.Lang
import           Ohua.Monad


runOptimizations :: (MonadError String m, MonadOhua m) => DFExpr -> m DFExpr
runOptimizations = return
