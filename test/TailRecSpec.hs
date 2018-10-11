module TailRecSpec (passesSpec) where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.Passes.TailRec (findTailRecs)
import qualified Ohua.ALang.Refs as ALangRefs

import Test.Hspec

-- FIXME copied from PassesSpec. put this in a test module.
instance Num ResolvedSymbol where fromInteger = Env . fromInteger
instance Num Expression where fromInteger = Var . fromInteger

-- FIXME copied from DFLowering. put this in a test module.
sf :: a -> AExpr bndType (Symbol a)
sf = Var . flip Sf Nothing

recWithExprOnTerminalBranch :: Expression
recWithExprOnTerminalBranch =
  (Let "a"
     (Lambda "i"
          (Let "p"
               (("math/-" `Apply` "i") `Apply` 10)
               (Let "x"
                    (("math/<" `Apply` "p") `Apply` 0)
                    (Let "c"
                         (Apply
                              (Apply
                                   (Apply (sf ALangRefs.ifThenElse) "x")
                                   (Lambda "then" (Let "t" (Apply (sf ALangRefs.id) "p")
                                                       "t")))
                                   (Lambda "else" (Let "r" ("a" `Apply` "p")
                                                       "r")))
                         "c"))))
     (Let "y" ("a" `Apply` 95) "y"))

recWithVarOnlyOnTerminalBranch :: Expression
recWithVarOnlyOnTerminalBranch =
   (Let "a"
      (Lambda "i"
           (Let "p"
                (("math/-" `Apply` "i") `Apply` 10)
                (Let "x"
                     (("math/<" `Apply` "p") `Apply` 0)
                     (Let "c"
                          (Apply
                               (Apply
                                    (Apply (sf ALangRefs.ifThenElse) "x")
                                    (Lambda "then" "p"))
                                    (Lambda "else" (Let "r" ("a" `Apply` "p")
                                                        "r")))
                          "c"))))
      (Let "y" ("a" `Apply` 95) "y"))

recWithExprOnRecurBranch :: Expression
recWithExprOnRecurBranch =
   (Let "a"
      (Lambda "i"
           (Let "p"
                (("math/-" `Apply` "i") `Apply` 10)
                (Let "x"
                     (("math/<" `Apply` "p") `Apply` 0)
                     (Let "c"
                          (Apply
                               (Apply
                                    (Apply (sf ALangRefs.ifThenElse) "x")
                                    (Lambda "then" "p"))
                                    (Lambda "else" (Let "r" ("a" `Apply` "p")
                                                        "r")))
                          "c"))))
      (Let "y" ("a" `Apply` 95) "y"))

recWithCallOnlyOnRecurBranch :: Expression
recWithCallOnlyOnRecurBranch =
   (Let "a"
      (Lambda "i"
           (Let "p"
                (("math/-" `Apply` "i") `Apply` 10)
                (Let "x"
                     (("math/<" `Apply` "p") `Apply` 0)
                     (Let "c"
                          (Apply
                               (Apply
                                    (Apply (sf ALangRefs.ifThenElse) "x")
                                    (Lambda "then" "p"))
                                    (Lambda "else" ("a" `Apply` "p")))
                          "c"))))
      (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithExprOnTerminalBranch :: Expression
expectedRecWithExprOnTerminalBranch =
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
                                             (Apply (sf ALangRefs.ifThenElse) "x")
                                             (Lambda "then" (Let "t" (Apply (sf ALangRefs.id) "p")
                                                                 "t")))
                                             (Lambda "else" (Let "r" ((sf ALangRefs.recur) `Apply` "p")
                                                                 "r")))
                                   "c"))))
               (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithVarOnlyOnTerminalBranch :: Expression
expectedRecWithVarOnlyOnTerminalBranch =
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
                                            (Apply (sf ALangRefs.ifThenElse) "x")
                                            (Lambda "then" "p"))
                                            (Lambda "else" (Let "r" ((sf ALangRefs.recur) `Apply` "p")
                                                                "r")))
                                  "c"))))
              (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithExprOnRecurBranch :: Expression
expectedRecWithExprOnRecurBranch =
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
                                            (Apply (sf ALangRefs.ifThenElse) "x")
                                            (Lambda "then" "p"))
                                            (Lambda "else" (Let "r" ((sf ALangRefs.recur) `Apply` "p")
                                                                "r")))
                                  "c"))))
              (Let "y" ("a" `Apply` 95) "y"))

expectedRecWithCallOnlyOnRecurBranch :: Expression
expectedRecWithCallOnlyOnRecurBranch =
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
                                            (Apply (sf ALangRefs.ifThenElse) "x")
                                            (Lambda "then" "p"))
                                            (Lambda "else" ((sf ALangRefs.recur) `Apply` "p")))
                                  "c"))))
              (Let "y" ("a" `Apply` 95) "y"))


detect_recursion recExpr expectedExpr = findTailRecs True recExpr `shouldBe` expectedExpr

passesSpec :: Spec
passesSpec = do
  describe "Phase 1: rec detection" $ do
      it "recursion with expression (let + id) on terminal branch" $
        detect_recursion recWithExprOnTerminalBranch expectedRecWithExprOnTerminalBranch
      it "recursion with var on terminal branch" $
        detect_recursion recWithVarOnlyOnTerminalBranch expectedRecWithVarOnlyOnTerminalBranch
      it "recursion with expression (let) on recursion branch" $
        detect_recursion recWithExprOnRecurBranch expectedRecWithExprOnRecurBranch
      it "recursion correctly structured" $
        detect_recursion recWithCallOnlyOnRecurBranch expectedRecWithCallOnlyOnRecurBranch

  -- describe "Phase 2: hoferizing rec"
  --     it "hoferizes simple recursion"
