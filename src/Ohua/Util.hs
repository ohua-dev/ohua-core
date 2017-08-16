-- |
-- Module      : $Header$
-- Description : Utility functions and type synonyms
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE Rank2Types #-}
module Ohua.Util where

import           Lens.Micro


type Prism s t a b = Traversal s t a b


type Prism' a b = Prism a a b b


prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism make get f thing =
    case get thing of
        Left thing' -> pure thing'
        Right b     -> make <$> f b


prism' :: (b -> a) -> (a -> Maybe b) -> Prism' a b
prism' make get = prism make (\s -> maybe (Left s) Right $ get s)
