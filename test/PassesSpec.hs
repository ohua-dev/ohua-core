-- |
-- Module      : $Header$
-- Description : Tests for transformation passes on the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module PassesSpec (passesSpec) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.Default
import           Data.Either
import           Data.Functor.Identity
import           Debug.Trace
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.ALang.Passes.TailRec
import qualified Ohua.ALang.Refs           as ALangRefs
import           Ohua.ALang.Show
import           Ohua.Monad
import           Ohua.Types                as Ty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Property  as P

import           Ohua.Types.Arbitrary


smapName :: QualifiedBinding
smapName = "ohua.lang/smap"

-- newtype ApplyOnlyExpr = ApplyOnlyExpr { unApplyOnlyExpr :: Expression } deriving (Eq, Show)

-- instance Arbitrary ApplyOnlyExpr where
--     arbitrary = ApplyOnlyExpr <$> sized tree
--       where
--         tree (_, values) 0 = Var $ Local

instance Num ResolvedSymbol where fromInteger = Env . fromInteger
instance Num Expression where fromInteger = Var . fromInteger


runPasses :: Expression -> IO (Either Ty.Error Expression)
runPasses expr
  = runM
  $ runError
  $ runSilentLogger
  $ flip (runFromExpr def) expr
  $ performSSA >=> (normalize >=> \e -> checkProgramValidity e >> return e)

type ALangCheck = Either String


everyLetBindsCall :: Expression -> ALangCheck ()
everyLetBindsCall (Let _ (Apply _ _) body) = everyLetBindsCall body
everyLetBindsCall (Let _ _ _)              = Left "Let without call"
everyLetBindsCall _                        = return ()


noNestedLets :: Expression -> ALangCheck ()
noNestedLets (Let _ expr1 body) = noLets expr1 >> noNestedLets body
noNestedLets _                  = return ()


noLets :: Expression -> ALangCheck ()
noLets (Let _ _ _)         = Left "Found a let"
noLets (Apply expr1 expr2) = noLets expr1 >> noLets expr2
noLets (Lambda _ expr)     = noLets expr
noLets _                   = return ()


checkInvariants :: Expression -> ALangCheck ()
checkInvariants expr = do
    noNestedLets expr
    everyLetBindsCall expr
    maybe (return ()) (Left . show) $ isSSA expr

prop_passes :: Expression -> Property
prop_passes input = ioProperty $ do
    () <- return $ input `deepseq` ()
    transformed <- runPasses input
    return $ case transformed of
        Right program ->
            case checkInvariants program of
                Left err -> failed { P.reason = err }
                Right _  -> succeeded
        _ -> succeeded { abort = True }


shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

rejects :: Expression -> Expectation
rejects e = runPasses e `shouldSatisfyRet` isLeft

doesn't_reject :: Expression -> Expectation
doesn't_reject e = runPasses e `shouldSatisfyRet` isRight

lambda_as_argument = Apply (Var (Sf smapName Nothing)) (Lambda "a" "a")

bound_lambda_as_argument = Let "f" (Lambda "a" "a") (Apply "some/function" "f")

calculated_lambda_as_argument = Let "f" (Lambda "a" "a") $ Let "z" (Lambda "a" "a") $ Let "g" (Apply "f" "z") $ (Apply "some/function" "g")

lambda_with_app_as_arg = Apply "some/func" $ Apply (Lambda "a" (Lambda "b" "a")) $ Var $ Env 10

passesSpec = do
    describe "normalization" $ do
        prop "creates ir with the right invariants" prop_passes
        it "does not reject a program with lambda as input to smap" $
            doesn't_reject lambda_as_argument

        it "doesn't reject a program where bound lambda is input" $
            doesn't_reject bound_lambda_as_argument

        it "doesn't reject a program where calculated lambda is input" $
            doesn't_reject calculated_lambda_as_argument

        let lambdaStaysInput e@(Apply _ (Lambda _ (Lambda _ _))) = False
            lambdaStaysInput e@(Apply (Var (Sf _ _)) (Lambda _ _)) = True
            lambdaStaysInput (Let _ expr _) = lambdaStaysInput expr
            lambdaStaysInput (Apply _ body) = lambdaStaysInput body
            lambdaStaysInput e = False

        it "Reduces lambdas as far as possible but does not remove them when argument" $
            runPasses lambda_with_app_as_arg `shouldSatisfyRet` either (const False) lambdaStaysInput

        let normalize' :: Expression -> IO (Either Ty.Error Expression)
            normalize' = runM . runError . runSilentLogger . runFromExpr def normalize
            e = Let "f" (Lambda "x" ("some/fn" `Apply` "x")) $
                Let "g" ("f" `Apply` "a")
                "g" `Apply` "b" :: Expression

        it "reduces curried functions which are produced by a lambda" $
            normalize' e `shouldReturn` Right (Let "c" ("some/fn" `Apply` "a" `Apply` "b" ) "c")

    describe "reassignment inlining" $ do
        it "inlines reassignments in argument lambdas" $
            inlineReassignments ("ohua.lang/smap" `Apply` Lambda "i_0" (Let "x_1" "i_0" ("ohua.math/add" `Apply` "x_1" `Apply` "x_1")) `Apply` "x")
            `shouldBe`
            ("ohua.lang/smap" `Apply` Lambda "i_0" ("ohua.math/add" `Apply` "i_0" `Apply` "i_0") `Apply` "x")
        it "inlines reassignments in argument lambdas in complex expressions" $
            inlineReassignments
                (Let "coll0" ("ohua.lang/smap" `Apply` Lambda "i" (Let "x" "i" ("ohua.math/add" `Apply` "x" `Apply` "x")) `Apply` "coll") "coll0")
            `shouldBe`
            Let "coll0" ("ohua.lang/smap" `Apply` Lambda "i" ("ohua.math/add" `Apply` "i" `Apply` "i") `Apply` "coll") "coll0"



    describe "remove currying pass" $ do
        let removeCurrying ::  Expression -> IO (Either Ty.Error Expression)
            removeCurrying = runM . runError . runSilentLogger . flip (runFromBindings def) mempty . Ohua.ALang.Passes.removeCurrying
        it "inlines curring" $
            removeCurrying (Let "a" ("mod/fun" `Apply` "b") ("a" `Apply` "c"))
            `shouldReturn`
            Right ("mod/fun" `Apply` "b" `Apply` "c")
        it "inlines curring 2" $
            removeCurrying (Let "a" ("mod/fun" `Apply` "b") $ Let "x" ("a" `Apply` "c") "x")
            `shouldReturn`
            Right (Let "x" ("mod/fun" `Apply` "b" `Apply` "c") "x")
        it "removes currying even for redefintions" $
            removeCurrying (Let "a" "some/sf" $ Let "b" "a" $ "b" `Apply` "c")
            `shouldReturn`
            Right ("some/sf" `Apply` "c")
        it "inlines multiple layers of currying" $
            removeCurrying
                (Let "f" (Apply "some-ns/fn-with-3-args" "a") $
                Let "f'" (Apply "f" "b") $
                Let "x" (Apply "f'" "c")
                "x" )
            `shouldReturn`
            Right (Let "x" ("some-ns/fn-with-3-args" `Apply` "a" `Apply` "b" `Apply` "c") "x")

    -- TODO
--    describe "lambda lifting" $ do
--        it "lifts simple lambda" $


    describe "letrec detection" $ do
        it "detects simple recusion" $
           findTailRecs (Let "a" (Lambda "i" (Let "p" (("math/-" `Apply` "i") `Apply` 10)
                                     (Let "x" (("math/<" `Apply` "p") `Apply` 0)
                                          (Let "c" (Apply (Apply (Apply (Var $ Sf ALangRefs.ifThenElse Nothing) "x")
                                                                                (Lambda "then" (Let "t" (Apply (Var $ Sf ALangRefs.id Nothing) "p")
                                                                                                    "t")))
                                                                                (Lambda "else" (Let "r" ("a" `Apply` "p")
                                                                                                    "r")))
                                                        "c"))))
                             (Let "y" ("a" `Apply` 95)
                                  "y")
                          )
           `shouldBe`
           (Let (Recursive (Binding "a")) (Lambda "i"
                                           (Let "p" (("math/-" `Apply` "i") `Apply` 10)
                                                (Let "x" (("math/<" `Apply` "p") `Apply` 0)
                                                     (Let "c" (Apply (Apply (Apply (Var $ Sf ALangRefs.ifThenElse Nothing) "x")
                                                                                   (Lambda "then" (Let "t" (Apply (Var $ Sf ALangRefs.id Nothing) "p")
                                                                                                       "t")))
                                                                                   (Lambda "else" (Let "r" ((Var $ Sf ALangRefs.recur Nothing) `Apply` "p")
                                                                                                       "r")))
                                                           "c"))))
                                (Let "y" ("a" `Apply` 95)
                                     "y")
                       )
