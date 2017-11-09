-- |
-- Module      : $Header$
-- Description : Optimizations performed on the algorithm language
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Optimizations where


import           Control.Monad.Except
import           Ohua.ALang.Lang
import           Ohua.Monad


runOptimizations :: Monad m => Expression -> m Expression
runOptimizations = return
