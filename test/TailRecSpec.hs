

module TailRecSpec (passesSpec) where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.Passes (normalize)
import Ohua.ALang.Passes.TailRec (findTailRecs, hoferize, recur, recur_hof, verifyTailRecursion)
import qualified Ohua.ALang.Refs as ALangRefs
import Ohua.ALang.PPrint (quickRender)

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
                                             (Lambda "else" (Let "r" ((sf recur) `Apply` "p")
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
                                            (Lambda "else" (Let "r" ((sf recur) `Apply` "p")
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
                                            (Lambda "else" (Let "r" ((sf recur) `Apply` "p")
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
                                            (Lambda "else" ((sf recur) `Apply` "p")))
                                  "c"))))
              (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive1 :: Expression
notTailRecursive1 =
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
                                    (Lambda "else"
                                      (Let "g" ((sf recur) `Apply` "p")
                                           ("math/times10" `Apply` "g"))
                                    ))
                          "c"))))
      (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive2 :: Expression
notTailRecursive2 =
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
                                    (Lambda "else" ((sf recur) `Apply` "p")))
                          ("math/times10" `Apply` "c")))))
      (Let "y" ("a" `Apply` 95) "y"))

notTailRecursive3 :: Expression
notTailRecursive3 =
  (Let (Recursive ("a" :: Binding))
             (Lambda
                  "i"
                  (Let "g"
                        (Let "p"
                             (("math/-" `Apply` "i") `Apply` 10)
                             (Let "x"
                                  (("math/<" `Apply` "p") `Apply` 0)
                                  (Let "c"
                                       (Apply
                                            (Apply
                                                 (Apply (sf ALangRefs.ifThenElse) "x")
                                                 (Lambda "then" "p"))
                                                 (Lambda "else" ((sf recur) `Apply` "p")))
                                       "c")))
                          ("math/times10" `Apply` "g")))
             (Let "y" ("a" `Apply` 95) "y"))


expectedHoferized :: Expression
expectedHoferized =
  (Let (Direct ("a_0" :: Binding))
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
                                             (Lambda "else" ((sf recur) `Apply` "p")))
                                   "c"))))
               (Let "a" ((sf recur_hof) `Apply` "a_0")
                    (Let "y" ("a" `Apply` 95) "y")))

detect_recursion recExpr expectedExpr = findTailRecs True recExpr `shouldBe` expectedExpr

runPass :: (Expression -> OhuaM env Expression) -> Expression -> IO (Either Error Expression)
runPass pass expr = runSilentLoggingT $ runFromExpr def pass expr

hof :: Expression -> Expression -> Expectation
hof expr expected = runPass hoferize expr >>= ((`shouldBe` (quickRender expected)) . quickRender . (fromRight (Var "test/failure")))

-- noTailRec :: Expression -> String -> Expectation
noTailRec expr expected = (runPass (hoferize >=> normalize >=> verifyTailRecursion) expr) `shouldThrow` (errorCall expected)


-- -- (let [a (fn [i] (let [p (math/- i 10)]
-- --                       (ohua.lang/if (math/< p 0)
-- --                                     p
-- --                                     (a p))))]
-- --       (a 95))
-- recurSpec :: Spec
-- recurSpec = do
--     describe "recur lowering" $ do
--         it "lowers a simple recursion" $
--             (Let (Recursive ("a" :: Binding))
--                  (Lambda
--                       "i"
--                       (Let "p"
--                            (("math/-" `Apply` "i") `Apply` 10)
--                            (Let "x"
--                                 (("math/<" `Apply` "p") `Apply` 0)
--                                 (Let "c"
--                                      (Apply
--                                           (Apply
--                                                (Apply
--                                                     (Var $
--                                                      Sf
--                                                          ALangRefs.ifThenElse
--                                                          Nothing)
--                                                     "x")
--                                                (Lambda
--                                                     "then"
--                                                     (Let "t"
--                                                          (Apply
--                                                               (Var $
--                                                                Sf
--                                                                    ALangRefs.id
--                                                                    Nothing)
--                                                               "p")
--                                                          "t")))
--                                           (Lambda
--                                                "else"
--                                                (Let "r"
--                                                     ((Var $
--                                                       Sf ALangRefs.recur Nothing) `Apply`
--                                                      "p")
--                                                     "r")))
--                                      "c"))))
--                  (Let "y" ("a" `Apply` 95) "y")) `shouldLowerTo`
--             DFExpr
--                 [ LetExpr 0 "i_0" Refs.id [DFEnvVar 95] Nothing -- this adapts the actual to the formal that goes into algo-in and then becomes "i"
--                 -- inside the lambda everything is left untouched.
--                 , LetExpr
--                       1
--                       "p"
--                       (EmbedSf "math/-")
--                       [DFVar "i", DFEnvVar 10]
--                       Nothing
--                 , LetExpr
--                       2
--                       "x"
--                       (EmbedSf "math/<")
--                       [DFVar "p", DFEnvVar 0]
--                       Nothing
--                 , LetExpr 3 ["then", "else"] Refs.bool [DFVar "x"] Nothing
--                 , LetExpr 4 ["p_0"] Refs.scope [DFVar "p"] $ Just "then"
--                 , LetExpr 5 "t" Refs.id [DFVar "p_0"] Nothing
--                 , LetExpr 6 ["p_1"] Refs.scope [DFVar "p"] $ Just "else"
--                 -- the two functions to gather the parameters for the call to recur
--                 , LetExpr 7 "recur-in_0" Refs.array [DFVar "p_1"] Nothing
--                 , LetExpr 8 "algo-in_0" Refs.array [DFVar "i_0"] Nothing
--                 -- note: recur produces finally the formal input vars of the lambda
--                 , LetExpr
--                       9
--                       ["i"]
--                       Refs.recur
--                       [DFVar "x", DFVar "algo-in_0", DFVar "recur-in_0"]
--                       Nothing
--                 , LetExpr 10 "y" Refs.id [DFVar "t"] Nothing -- this adapts the output formal to the output actual
--                 ]
--                 "y"


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

  describe "Phase 2: hoferizing rec" $ do
      it "hoferizes correct recursion" $
        hof expectedRecWithCallOnlyOnRecurBranch expectedHoferized

  describe "Verification:" $ do
    it "no tail recursion 1" $
      noTailRec notTailRecursive1 "Recursion is not tail recursive!"
    it "no tail recursion 2" $
      noTailRec notTailRecursive2 "Recursion is not tail recursive! Last stmt: \"math/times10 c\""
    it "no tail recursion 3" $
      noTailRec notTailRecursive3 "Recursion is not tail recursive! Last stmt: \"math/times10 g\""
