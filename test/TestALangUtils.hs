{-# LANGUAGE OverloadedStrings #-}
module TestALangUtils where


import Ohua.ALang.Lang
import Test.Hspec


substitute' var val = lrPostwalkExpr f
  where
    f (Var (Local v)) | var == v = val
    f e               = e


utilsSpec :: Spec
utilsSpec = do
    describe "substitute with postwalk" $ do
        it "substitutes a single var" $
            substitute' "v" "testval" "v" `shouldBe` "testval"
        it "substitutes a complicated expression" $
            let toSub@(Var (Local toSubBinding)) = "h"
                subVal = "result" `Apply` "e"
                e = 
                    Let "x" toSub $
                    Let "v" (toSub `Apply` "c") $
                    Let "q" (Lambda "g" ("some/func" `Apply` toSub))
                    "q"
                
                e1 = 
                    Let "x" subVal $
                    Let "v" (subVal `Apply` "c") $
                    Let "q" (Lambda "g" ("some/func" `Apply` subVal))
                    "q"
            in substitute' toSubBinding subVal e `shouldBe` e1
        
        it "does not recurse indefinitely" $
            substitute' "h" "h" "h" `shouldBe` "h"

        
spec = utilsSpec
