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
{-# LANGUAGE OverloadedStrings #-}
module PassesSpec (passesSpec) where

import           Control.DeepSeq
import           Control.Monad.Except
import           Data.Either
import           Debug.Trace
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.ALang.Show
import           Ohua.IR.Functions
import           Ohua.Monad
import           Ohua.Types
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Property as P

import Ohua.Types.Arbitrary


-- newtype ApplyOnlyExpr = ApplyOnlyExpr { unApplyOnlyExpr :: Expression } deriving (Eq, Show)

-- instance Arbitrary ApplyOnlyExpr where
--     arbitrary = ApplyOnlyExpr <$> sized tree
--       where
--         tree (_, values) 0 = Var $ Local


runPasses expr = flip runOhuaT expr $ performSSA >=> runExceptT . (normalize >=> \e -> checkProgramValidity e >> return e)

type ALangCheck = Either String


everyLetBindsCall :: Expression -> ALangCheck ()
everyLetBindsCall (Let _ (Apply _ _) body) = everyLetBindsCall body
everyLetBindsCall (Let _ _ _)              = throwError "Let without call"
everyLetBindsCall _                        = return ()


noNestedLets :: Expression -> ALangCheck ()
noNestedLets (Let _ expr1 body) = noLets expr1 >> noNestedLets body
noNestedLets _                  = return ()


noLets :: Expression -> ALangCheck ()
noLets (Let _ _ _)         = throwError "Found a let"
noLets (Apply expr1 expr2) = noLets expr1 >> noLets expr2
noLets (Lambda _ expr)     = noLets expr
noLets _                   = return ()


checkInvariants :: Expression -> ALangCheck ()
checkInvariants expr = do
    noNestedLets expr
    everyLetBindsCall expr
    hasFinalLet expr
    maybe (return ()) (throwError . show) $ isSSA expr

prop_passes :: Expression -> Property
prop_passes input = ioProperty $ do
    () <- return $ input `deepseq` ()
    transformed <- runPasses input
    return $ case transformed of
        Left msg -> succeeded { abort = True }
        Right program ->
            case checkInvariants program of
                Left err -> failed { P.reason = err }
                Right _  -> succeeded


shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

rejects :: Expression -> Expectation
rejects e = runPasses e `shouldSatisfyRet` isLeft

doesn't_reject :: Expression -> Expectation
doesn't_reject e = runPasses e `shouldSatisfyRet` isRight

lambda_as_argument = Apply (Var (Sf smapName Nothing)) (Lambda "a" "a")

bound_lambda_as_argument = Let "f" (Lambda "a" "a") (Apply "some/function" "f")

calculated_lambda_as_argument = Let "f" (Lambda "a" "a") $ Let "z" (Lambda "a" "a") $ Let "g" (Apply "f" "z") $ (Apply "some/function" "g")

lambda_with_app_as_arg = Apply "some/func" $ Apply (Lambda "a" (Lambda "b" "a")) "b"

passesSpec = do
    describe "normalization" $ do
        prop "creates ir with the right invariants" prop_passes
        it "does not reject a program with lambda as input to smap" $
            doesn't_reject lambda_as_argument

        it "doesn't reject a program where bound lambda is input" $
            doesn't_reject bound_lambda_as_argument

        it "doesn't reject a program where calculated lambda is input" $
            doesn't_reject calculated_lambda_as_argument

        let lambdaStaysInput (Apply _ (Lambda _ (Lambda _ _))) = False
            lambdaStaysInput (Apply (Var (Sf _ _)) (Lambda _ _)) = True
            lambdaStaysInput (Let _ expr _) = lambdaStaysInput expr
            lambdaStaysInput (Apply _ body) = lambdaStaysInput body
            lambdaStaysInput _ = False

        it "Reduces lambdas as far as possible but does not remove them when argument" $
            runPasses lambda_with_app_as_arg `shouldSatisfyRet` either (const False) lambdaStaysInput

        let normalize' = runOhuaT normalize
            e = Let "f" (Lambda "x" ("some/fn" `Apply` "x")) $
                Let "g" ("f" `Apply` "a")
                "g" `Apply` "b"

        it "reduces curried functions which are produced by a lambda" $
            normalize' e `shouldBe` Right (Let "c" ("some/fn" `Apply` "a" `Apply` "b" ) "c")



    describe "remove currying pass" $ do
        it "inlines curring" $
            removeCurrying (Let "a" ("mod/fun" `Apply` "b") ("a" `Apply` "c"))
            `shouldBe`
            Right ("mod/fun" `Apply` "b" `Apply` "c")
        it "inlines curring 2" $
            removeCurrying (Let "a" ("mod/fun" `Apply` "b") $ Let "x" ("a" `Apply` "c") "x")
            `shouldBe`
            Right (Let "x" ("mod/fun" `Apply` "b" `Apply` "c") "x")
        it "removes currying even for redefintions" $
            removeCurrying (Let "a" "some/sf" $ Let "b" "a" $ "b" `Apply` "c")
            `shouldBe`
            Right ("some/sf" `Apply` "c")
        it "inlines multiple layers of currying" $
            removeCurrying
                (Let "f" (Apply "some-ns/fn-with-3-args" "a") $
                Let "f'" (Apply "f" "b") $
                Let "x" (Apply "f'" "c")
                "x" )
            `shouldBe`
            Right (Let "x" ("some-ns/fn-with-3-args" `Apply` "a" `Apply` "b" `Apply` "c") "x")
