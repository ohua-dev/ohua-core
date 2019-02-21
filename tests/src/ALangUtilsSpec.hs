{-# LANGUAGE QuasiQuotes #-}

module ALangUtilsSpec where


import Ohua.Prelude

import Test.Hspec

import Ohua.ALang.Lang
import Ohua.ALang.Util (lambdaLifting)
import Ohua.Test (embedALang)

-- substitute' :: Binding -> Expression -> Expression -> Expression
-- substitute' var val = lrPostwalkExpr f
--   where
--     f (Var (Local v)) | var == v = val
--     f e               = e
simpleLift = [embedALang| \a -> some/sfn a c |]

expectedSimpleLift = ([embedALang| \a c_0 -> some/sfn a c_0 |], ["c"])

simpleLiftWithLet = [embedALang| \a -> let b = some/sfn a c in b |]

expectedSimpleLiftWithLet =
    ([embedALang| \a c_0 -> let b = some/sfn a c_0 in b |], ["c"])

moreComplexExpr =
    [embedALang| \a b ->
                                    let c = some/computation a d
                                    in if c
                                       then something.on.true/branch b e
                                       else something.on.false/branch f
                  |]

expectedMoreComplexExpr =
    ( [embedALang| \a b d_0 e_0 f_0 ->
                                   let c = some/computation a d_0
                                   in if c
                                      then something.on.true/branch b e_0
                                      else something.on.false/branch f_0
                            |]
    , ["d", "e", "f"])

liftLambda expr expected =
    (runSilentLoggingT $ runFromExpr def lambdaLifting expr) >>=
    ((`shouldBe` expected) .
     (fromRight ([embedALang| some.failure/happened |], [])))

utilsSpec :: Spec
utilsSpec
    -- describe "substitute with postwalk" $ do
        -- it "substitutes a single var" $
        --     substitute' "v" "testval" "v" `shouldBe` "testval"
        -- it "substitutes a complicated expression" $
        --     let toSubBinding = "h"
        --         toSub = Var $ Local toSubBinding
        --         subVal = "result" `Apply` "e"
        --         e =
        --             Let "x" toSub $
        --             Let "v" (toSub `Apply` "c") $
        --             Let "q" (Lambda "g" ("some/func" `Apply` toSub))
        --             "q"
        --
        --         e1 =
        --             Let "x" subVal $
        --             Let "v" (subVal `Apply` "c") $
        --             Let "q" (Lambda "g" ("some/func" `Apply` subVal))
        --             "q"
        --     in substitute' toSubBinding subVal e `shouldBe` e1
        --
        -- it "does not recurse indefinitely" $
        --     substitute' "h" "h" "h" `shouldBe` "h"
 = do
    describe "lambda lifting" $ do
        it "lambda lift single free var in function application" $
            liftLambda simpleLift expectedSimpleLift
        it
            "lambda lift single free var in function application (with let binding)" $
            liftLambda simpleLiftWithLet expectedSimpleLiftWithLet
        it "a more complex expression with conditionals" $
            liftLambda moreComplexExpr expectedMoreComplexExpr

spec :: Spec
spec = utilsSpec
