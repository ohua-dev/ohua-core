{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Data.Foldable
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.ALang.Show
import           Ohua.Monad


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
    Apply (Lambda (Direct "c") (Var (Sf "com.ohua.lang/smap-io-fun" Nothing))) (Lambda (Direct "c") (Var (Algo "com.ohua.lang/pcollect")))


main = do
    for_ [expr, expr0, expr1] $ \expr -> do
        let inlidedRefs = either error id $ inlineLambdaRefs expr
            inlined = inlineLambda inlidedRefs
        putStrLn $ showLambda expr
        putStrLn $ showLambda inlidedRefs
        putStrLn $ showLambda inlined

    res <- runOhuaC mkSSA expr2
    putStrLn $ showLambda res
    print $ isSSA expr2
    print $ isSSA res

    putStrLn $ showLambda expr3

    processed <- flip runOhuaC expr3 (mkSSA >=> runExceptT . normalize)
    putStrLn $ either id showLambda $ processed

    putStrLn $ showLambda $ letLift $ inlineLambda expr4

    processed <- flip runOhuaC expr5 (mkSSA >=> runExceptT . normalize)
    putStrLn $ showLambda expr5
    putStrLn $ either id showLambda $ processed

