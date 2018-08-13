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

import Protolude

import Data.Functor.Foldable

import Ohua.ALang.Lang
import Ohua.Types
import qualified Ohua.ALang.Refs as Refs



runOptimizations :: Monad m => Expression -> m Expression
runOptimizations = return


-- | Removes id calls which don't destructure.
removeId :: Expression -> Expression
removeId =
    para $ \case
        LetF bnd@(Direct _) (Var (Sf func _) `Apply` somevalue, _) (rest, _)
            | func == Refs.id -> Let bnd somevalue rest
        other -> embed $ snd <$> other
