{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Protolude

import           Ohua.ALang.Lang
import           Ohua.Types


expr, expr0, expr1, expr2, expr3, expr4, expr5 :: Expression
expr =
    Apply
        (Let "x" (Lambda "y" "y") "x")
        "g"

expr0 =
    Apply
        (Let "x" (Let "z" "p/something" (Lambda "y" "y")) "x")
        "g"

expr1 =
    Apply
        (Let "x" (Let "z" "p/something" "y") "x")
        "g"

expr2 =
    Let "x" (Lambda "x" "x")
        (Apply (Lambda "x" "x") "x")


expr3 =
    Apply
        (Let "x" "p"
            (Lambda
                "a"
                (Apply (Apply (Let "x" "something/something" "x") "x") "a")))
        (Let "a" "b"
            (Let "c" "a"
                (Apply
                    (Let "g" "namespace/function"
                        (Lambda "x"
                            (Apply "g" "c")))
                    "c")))


expr4 =
    Let "x" (Let "y" (Let "z" "a" "z") "y")
        "x"

expr5 =
    Apply
        (Lambda (Direct "c") (Var (Sf "com.ohua.lang/smap-io-fun" Nothing)))
        (Lambda (Direct "c") (Var (Sf "com.ohua.lang/pcollect" Nothing)))


main :: IO ()
main = return ()
