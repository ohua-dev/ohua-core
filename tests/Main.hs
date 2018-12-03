{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.Test (embedALang)

expr, expr0, expr1, expr2, expr3, expr4, expr5 :: Expression
-- expr = Apply (Let "x" (Lambda "y" "y") "x") "g"
expr = [embedALang| (let x = \y -> y in x) g|]

-- expr0 = Apply (Let "x" (Let "z" "p/something" (Lambda "y" "y")) "x") "g"
expr0 = [embedALang| (let x = (let z = p/something in (\y -> y)) in x) g |]

-- expr1 = Apply (Let "x" (Let "z" "p/something" "y") "x") "g"
expr1 = [embedALang| (let x = (let z = p/something in y) in x) g |]

-- expr2 = Let "x" (Lambda "x" "x") (Apply (Lambda "x" "x") "x")
expr2 = [embedALang| let x = \x -> x in (\x -> x) x |]

-- expr3 =
--     Apply
--         (Let "x"
--              "p"
--              (Lambda
--                   "a"
--                   (Apply (Apply (Let "x" "something/something" "x") "x") "a")))
--         (Let "a"
--              "b"
--              (Let "c"
--                   "a"
--                   (Apply
--                        (Let "g"
--                             "namespace/function"
--                             (Lambda "x" (Apply "g" "c")))
--                        "c")))
expr3 =
    [embedALang|
      (let x = p in
        \a -> (let x = something/something in x) x a)
      (let a = b in
        (let c = a in (let g = namespace/function in \x -> g c) c))
 |]

-- expr4 = Let "x" (Let "y" (Let "z" "a" "z") "y") "x"
expr4 = [embedALang| let x = (let y = (let z = a in z) in y) in x |]

expr5 =
    Apply
        (Lambda "c" "com.ohua.lang/smap-io-fun")
        (Lambda "c" "com.ohua.lang/pcollect")

-- expr5 =
--     [embedALang| (\c -> com.ohua.lang/smap-io-fun) (\c -> com.ohua.lang/pcollect) |]
main :: IO ()
main = return ()
