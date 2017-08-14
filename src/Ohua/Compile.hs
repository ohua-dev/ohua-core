{-# LANGUAGE CPP #-}
module Ohua.Compile where


import Ohua.ALang.Lang
import Ohua.ALang.Passes.SSA
import Ohua.ALang.Passes
import Ohua.DFLang.Lang
import Ohua.DFLang.Passes
import Ohua.Monad
import Ohua.DFGraph
import Lens.Micro
import Control.Monad.Except
import Ohua.ALang.Optimizations
import Ohua.DFLang.Optimizations



pipeline :: (MonadError String m, MonadOhua m) => Expression -> m OutGraph
pipeline e 
    = performSSA e
    >>= normalize
#ifdef DEBUG
    >>= \e -> Ohua.ALang.Passes.SSA.checkSSA e >> return e
#endif
    >>= Ohua.ALang.Optimizations.runOptimizations
#ifdef DEBUG
    >>= \e -> Ohua.ALang.Passes.SSA.checkSSA e >> return e
#endif
    >>= lowerALang
#ifdef DEBUG
    >>= \e -> Ohua.DFLang.Passes.checkSSA e >> return e
#endif
    >>= Ohua.DFLang.Optimizations.runOptimizations

#ifdef DEBUG
    >>= \e -> Ohua.DFLang.Passes.checkSSA e >> return e
#endif
    -- Comment: I use `<&>` (aka `fmap`) here because `toGraph` does not run in a monad
    <&> toGraph


compile :: Expression -> IO (Either String OutGraph)
compile e = flip runOhuaC e $ runExceptT . pipeline
    