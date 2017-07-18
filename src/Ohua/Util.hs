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
