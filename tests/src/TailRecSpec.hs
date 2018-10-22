{-# LANGUAGE LambdaCase #-}

module TailRecSpec (passesSpec) where

import Ohua.Prelude

import Ohua.Unit
import Ohua.ALang.Lang
import Ohua.ALang.Passes (normalize)
import Ohua.ALang.Passes.TailRec (findTailRecs, hoferize, recur, recur_hof, verifyTailRecursion, rewrite)
import qualified Ohua.ALang.Refs as ALangRefs
import Ohua.ALang.PPrint (quickRender)
import Ohua.DFLang.Lang
import Ohua.DFLang.Passes (lowerALang)

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

expectedRewritten :: Expression
expectedRewritten =
  (Let "y" (((sf recur)
             `Apply`
             (Lambda "e"
               (Let (Destructure [("i" :: Binding)]) "e"
                  (Let "p"
                       (("math/-" `Apply` "i") `Apply` 10)
                       (Let "x"
                            (("math/<" `Apply` "p") `Apply` 0)
                            (Let "c"
                                 (Apply
                                      (Apply
                                           (Apply (sf ALangRefs.ifThenElse) "x")
                                           (Lambda "then"
                                              (Let "d"
                                                   (((sf ALangRefs.mkTuple) `Apply` ((sf ALangRefs.false) `Apply` unitExpr))
                                                   `Apply`
                                                   ((sf ALangRefs.id) `Apply` "p"))
                                                   "d")
                                              ))
                                           (Lambda "else"
                                              (Let "b"
                                                   (((sf ALangRefs.mkTuple) `Apply` ((sf ALangRefs.true) `Apply` unitExpr))
                                                   `Apply`
                                                   ((sf ALangRefs.array) `Apply` "p"))
                                                   "b")
                                              ))
                                 "c")))))
             )
            `Apply`
            ((sf ALangRefs.array) `Apply` 95))
        "y")

-- TODO It would be sooo nice, if I could write my test expressions like this!
-- let y = ohua.lang/recur (\955 e ->
--                                 let (i) = e
--                                 in let p = math/- i $10
--                                    in let x = math/< p $0
--                                       in let c = ohua.lang/if x
--                                                              (\955 then ->
--                                                                        let d = ohua.lang/(,) (ohua.lang/false ()) (ohua.lang/id p)
--                                                                        in d)
--                                                              (\955 else ->
--                                                                        let b = ohua.lang/(,) (ohua.lang/true ()) (ohua.lang/array p)
--                                                                        in b)
--                                         in c)
--                         (ohua.lang/array $95)
-- in y

expectedLowered =
  DFExpr
    [ LetExpr 1  "f"              (EmbedSf "ohua.lang/array")     [DFEnvVar 95]                      Nothing
    , LetExpr 3  ["i"]            (EmbedSf "ohua.lang/id")        [DFVar "e"]                        Nothing
    , LetExpr 4  "p"              (EmbedSf "math/-")              [DFVar "i", DFEnvVar 10]           Nothing
    , LetExpr 5  "x"              (EmbedSf "math/<")              [DFVar "p", DFEnvVar 0]            Nothing
    , LetExpr 13 ["then", "else"] (EmbedSf "ohua.lang/bool")      [DFVar "x"]                        Nothing
    , LetExpr 14 ["p_0"]          (DFFunction "ohua.lang/scope")  [DFVar "p"]                        $ Just "then"
    , LetExpr 7  "g"              (EmbedSf "ohua.lang/id")        [DFVar "p_0"]                      Nothing
    , LetExpr 8  "h"              (EmbedSf "ohua.lang/false")     [dfVarUnit]                      $ Just "then"
    , LetExpr 9  "d"              (EmbedSf "ohua.lang/(,)")       [DFVar "h",DFVar "g"]              Nothing
    , LetExpr 15 ["p_1"]          (DFFunction "ohua.lang/scope")  [DFVar "p"]                        $ Just "else"
    , LetExpr 10 "j"              (EmbedSf "ohua.lang/array")     [DFVar "p_1"]                      Nothing
    , LetExpr 11 "k"              (EmbedSf "ohua.lang/true")      [dfVarUnit]                      $ Just "else"
    , LetExpr 12 "b"              (EmbedSf "ohua.lang/(,)")       [DFVar "k",DFVar "j"]              Nothing
    , LetExpr 16 "c"              (DFFunction "ohua.lang/select") [DFVar "then",DFVar "d",DFVar "b"] Nothing
    , LetExpr 17 ["y","e"]        (DFFunction "ohua.lang/recur")  [DFVar "f",DFVar "c"]              Nothing
    ]
  "y"

-- Runs:

detect_recursion recExpr expectedExpr = findTailRecs True recExpr `shouldBe` expectedExpr

runPass pass expr = runSilentLoggingT $ runFromExpr def pass expr

hof :: Expression -> Expression -> Expectation
hof expr expected = runPass hoferize expr >>= ((`shouldBe` (quickRender expected)) . quickRender . (fromRight (Var "test/failure")))

noTailRec expr expected = (runPass (hoferize >=> normalize >=> verifyTailRecursion) expr) `shouldThrow` (errorCall expected)

rewritePass expr expected = runPass
                              (hoferize >=> normalize >=> verifyTailRecursion >=> rewrite)
                              expr
                            >>=
                            ((`shouldBe` (quickRender expected)) . quickRender . (fromRight (Var "test/failure")))

lower expr expected = runPass
                        (hoferize >=> normalize >=> verifyTailRecursion >=> rewrite >=> normalize >=> lowerALang)
                        expr
                        >>=
                        ((`shouldBe` expected) .
                          (\case
                            Left err -> DFExpr [ LetExpr 0 "i" (EmbedSf "test/failure") [] Nothing ] (show err);
                            Right e -> e))


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

  describe "Phase 3: final ALang rewrite" $ do
      it "rewrites correct recursion" $
        rewritePass expectedRecWithCallOnlyOnRecurBranch expectedRewritten

  describe "Phase 4: DF lowering" $ do
      it "lowers correct recursion" $
        lower expectedRecWithCallOnlyOnRecurBranch expectedLowered
