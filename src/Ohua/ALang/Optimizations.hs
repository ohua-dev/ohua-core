module Ohua.ALang.Optimizations where


import           Control.Monad.Except
import           Ohua.ALang.Lang
import           Ohua.Monad


runOptimizations :: (MonadError String m, MonadOhua m) => Expression -> m Expression
runOptimizations = return
