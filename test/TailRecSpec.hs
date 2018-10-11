module TailRecSpec (passesSpec) where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.Passes.TailRec (findTailRecs)
import qualified Ohua.ALang.Refs as ALangRefs

import Test.Hspec

-- FIXME copied from PassesSpec. put this in a test module
instance Num ResolvedSymbol where fromInteger = Env . fromInteger
instance Num Expression where fromInteger = Var . fromInteger

passesSpec :: Spec
passesSpec = do
  -- Testing Phase 1
  describe "rec detection" $ do
      it "detects simple recusion" $
          findTailRecs True
              (Let "a"
                   (Lambda
                        "i"
                        (Let "p"
                             (("math/-" `Apply` "i") `Apply` 10)
                             (Let "x"
                                  (("math/<" `Apply` "p") `Apply` 0)
                                  (Let "c"
                                       (Apply
                                            (Apply
                                                 (Apply
                                                      (Var $
                                                       Sf
                                                           ALangRefs.ifThenElse
                                                           Nothing)
                                                      "x")
                                                 (Lambda
                                                      "then"
                                                      (Let "t"
                                                           (Apply
                                                                (Var $
                                                                 Sf
                                                                     ALangRefs.id
                                                                     Nothing)
                                                                "p")
                                                           "t")))
                                            (Lambda
                                                 "else"
                                                 (Let "r"
                                                      ("a" `Apply` "p")
                                                      "r")))
                                       "c"))))
                   (Let "y" ("a" `Apply` 95) "y")) `shouldBe`
          (Let (Recursive ("a" :: Binding))
               (Lambda
                    "i"
                    (Let "p"
                         (("math/-" `Apply` "i") `Apply` 10)
                         (Let "x"
                              (("math/<" `Apply` "p") `Apply` 0)
                              (Let "c"
                                   (Apply
                                        (Apply
                                             (Apply
                                                  (Var $
                                                   Sf
                                                       ALangRefs.ifThenElse
                                                       Nothing)
                                                  "x")
                                             (Lambda
                                                  "then"
                                                  (Let "t"
                                                       (Apply
                                                            (Var $
                                                             Sf
                                                                 ALangRefs.id
                                                                 Nothing)
                                                            "p")
                                                       "t")))
                                        (Lambda
                                             "else"
                                             (Let "r"
                                                  ((Var $
                                                    Sf ALangRefs.recur Nothing) `Apply`
                                                   "p")
                                                  "r")))
                                   "c"))))
               (Let "y" ("a" `Apply` 95) "y"))
