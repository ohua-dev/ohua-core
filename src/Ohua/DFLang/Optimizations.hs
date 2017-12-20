-- |
-- Module      : $Header$
-- Description : Optimizations performed on the dataflow language
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFLang.Optimizations where


import           Ohua.DFLang.Lang
import           Ohua.Monad


runOptimizations :: MonadOhua envExpr m => DFExpr -> m DFExpr
runOptimizations = return
