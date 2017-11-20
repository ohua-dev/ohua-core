{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Ohua.Monad
import Data.Default


main = print =<<
    runFromBindings def m []
  where
    m = sequence $ replicate 4 $ generateBindingWith "_"
