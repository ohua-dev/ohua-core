module Ohua.ALang.Optimizations where


import Ohua.Monad
import Control.Monad.Except
import Ohua.ALang.Lang
import Ohua.ALang.Passes


runOptimizations :: (MonadError String m, MonadOhua m) => Expression -> m Expression
runOptimizations = return
