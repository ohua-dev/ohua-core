-- |
-- Module      : $Header$
-- Description : Tests for transformation passes on the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
#endif
module PassesSpec
    ( spec
    ) where

import Ohua.Prelude

import Control.Comonad
import Data.Functor.Foldable (para)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as P

import Ohua.ALang.Lang
import Ohua.ALang.Passes
import Ohua.ALang.Passes.SSA
import qualified Ohua.ALang.Refs as ALangRefs
import Ohua.Test
import Ohua.Types.Arbitrary ()

smapName :: QualifiedBinding
smapName = "ohua.lang/smap"

-- newtype ApplyOnlyExpr = ApplyOnlyExpr { unApplyOnlyExpr :: Expression } deriving (Eq, Show)
-- instance Arbitrary ApplyOnlyExpr where
--     arbitrary = ApplyOnlyExpr <$> sized tree
--       where
--         tree (_, values) 0 = Var $ Local
instance Num Lit where
    fromInteger = EnvRefLit . fromInteger

instance Num Expression where
    fromInteger = Lit . fromInteger

runPasses :: Expression -> IO (Either Error Expression)
runPasses expr =
    runSilentLoggingT $
    flip (runFromExpr def) expr $
    performSSA >=> (normalize >=> \e -> checkProgramValidity e >> return e)

type ALangCheck = Either Text

everyLetBindsCall :: Expression -> ALangCheck ()
everyLetBindsCall (Let _ (Apply _ _) body) = everyLetBindsCall body
everyLetBindsCall Let {} = throwErrorS "Let without call"
everyLetBindsCall _ = return ()

noNestedLets :: Expression -> ALangCheck ()
noNestedLets (Let _ expr1 body) = noLets expr1 >> noNestedLets body
noNestedLets _ = return ()

noLets :: Expression -> ALangCheck ()
noLets e@Let {} = throwErrorS $ "Found a let " <> show e
noLets (Apply expr1 expr2) = noLets expr1 >> noLets expr2
noLets (Lambda _ expr) = noNestedLets expr
noLets _ = return ()

checkInvariants :: Expression -> ALangCheck ()
checkInvariants expr = do
    noNestedLets expr
    everyLetBindsCall expr
    case isSSA expr of
        [] -> return ()
        xs -> throwErrorS $ show xs

prop_passes :: Expression -> Property
prop_passes input =
    ioProperty $ do
        () <- return $ input `deepseq` ()
        transformed <- runPasses input
        return $
            case transformed of
                Right program ->
                    case checkInvariants program of
                        Left err ->
                            failed
                                { P.reason =
                                      toString $
                                      T.unlines
                                          [ err
                                          , "Input:"
                                          , show input
                                          , "Result:"
                                          , show program
                                          ]
                                }
                        Right _ -> succeeded
                _ -> succeeded {abort = True}

shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

-- rejects :: Expression -> Expectation
-- rejects e = runPasses e `shouldSatisfyRet` isLeft
doesn't_reject :: Expression -> Expectation
doesn't_reject e = runPasses e `shouldSatisfyRet` isRight

lambda_as_argument :: Expression
lambda_as_argument = Apply (PureFunction smapName Nothing) (Lambda "a" "a")

bound_lambda_as_argument :: Expression
bound_lambda_as_argument = Let "f" (Lambda "a" "a") (Apply "some/function" "f")

calculated_lambda_as_argument :: Expression
calculated_lambda_as_argument =
    Let "f" (Lambda "a" "a") $
    Let "z" (Lambda "a" "a") $
    Let "g" (Apply "f" "z") $ (Apply "some/function" "g")

lambda_with_app_as_arg :: Expression
lambda_with_app_as_arg =
    Apply "some/func" $ Apply (Lambda "a" (Lambda "b" "a")) $ 10

spec :: Spec
spec = do
    describe "normalization" $
        -- Add these tests back once we have a good generation for random programs
        -- prop "creates ir with the right invariants" prop_passes
     do
        it "does not reject a program with lambda as input to smap" $
            doesn't_reject lambda_as_argument
        it "doesn't reject a program where bound lambda is input" $
            doesn't_reject bound_lambda_as_argument
        it "doesn't reject a program where calculated lambda is input" $
            doesn't_reject calculated_lambda_as_argument
        let lambdaStaysInput =
                para $ \case
                    ApplyF _ (Lambda _ (Lambda _ _), _) -> False
                    ApplyF (PureFunction _ _, _) (Lambda _ _, _) -> True
                    LetF _ (extract -> expr) (extract -> body) -> expr || body
                    ApplyF _ (extract -> body) -> body
                    _ -> False
        it
            "Reduces lambdas as far as possible but does not remove them when argument" $
            runPasses lambda_with_app_as_arg `shouldSatisfyRet`
            either (const False) lambdaStaysInput
        let normalize' = runSilentLoggingT . runFromExpr def normalize
            e =
                Let "f" (Lambda "x" ("some/fn" `Apply` "x")) $
                Let "g" ("f" `Apply` "a") "g" `Apply` "b" :: Expression
        it "reduces curried functions which are produced by a lambda" $
            normalize' e `shouldReturn`
            Right (Let "c" ("some/fn" `Apply` "a" `Apply` "b") "c")
    describe "reassignment inlining" $ do
        it "inlines reassignments in argument lambdas" $
            inlineReassignments
                ("ohua.lang/smap" `Apply`
                 Lambda
                     "i_0"
                     (Let "x_1"
                          "i_0"
                          ("ohua.math/add" `Apply` "x_1" `Apply` "x_1")) `Apply`
                 "x") `shouldBe`
            ("ohua.lang/smap" `Apply`
             Lambda "i_0" ("ohua.math/add" `Apply` "i_0" `Apply` "i_0") `Apply`
             "x")
        it "inlines reassignments in argument lambdas in complex expressions" $
            inlineReassignments
                (Let "coll0"
                     ("ohua.lang/smap" `Apply`
                      Lambda
                          "i"
                          (Let "x" "i" ("ohua.math/add" `Apply` "x" `Apply` "x")) `Apply`
                      "coll")
                     "coll0") `shouldBe`
            Let
                "coll0"
                ("ohua.lang/smap" `Apply`
                 Lambda "i" ("ohua.math/add" `Apply` "i" `Apply` "i") `Apply`
                 "coll")
                "coll0"
    describe "remove currying pass" $ do
        let runRemoveCurrying =
                fmap (fmap showWithPretty) .
                runSilentLoggingT .
                flip (runFromBindings def) mempty .
                (Ohua.ALang.Passes.removeCurrying . inlineReassignments)
        let isSuccess = Right . showWithPretty
        it "inlines curring" $
            runRemoveCurrying
                (Let "a" ("mod/fun" `Apply` "b") ("a" `Apply` "c")) `shouldReturn`
            isSuccess ("mod/fun" `Apply` "b" `Apply` "c")
        it "inlines curring 2" $
            runRemoveCurrying
                (Let "a" ("mod/fun" `Apply` "b") $ Let "x" ("a" `Apply` "c") "x") `shouldReturn`
            isSuccess (Let "x" ("mod/fun" `Apply` "b" `Apply` "c") "x")
        it "removes currying even for redefintions" $
            runRemoveCurrying
                (Let "a" "some/sf" $ Let "b" "a" $ "b" `Apply` "c") `shouldReturn`
            isSuccess ("some/sf" `Apply` "c")
        it "inlines multiple layers of currying" $
            runRemoveCurrying
                (Let "f" (Apply "some-ns/fn-with-3-args" "a") $
                 Let "f'" (Apply "f" "b") $ Let "x" (Apply "f'" "c") "x") `shouldReturn`
            isSuccess
                (Let "x"
                     ("some-ns/fn-with-3-args" `Apply` "a" `Apply` "b" `Apply`
                      "c")
                     "x")
    describe "removing destructuring" $ do
        let mkNth0 objBnd i total =
                PureFunction ALangRefs.nth Nothing `Apply` Lit (NumericLit i) `Apply`
                Lit (NumericLit total) `Apply`
                Var objBnd
            runRemDestr = pure
        it "removes destructuring from lets" $
            let objBnd = "d"
                mkNth = mkNth0 objBnd
             in runRemDestr [embedALang| let (a, b, c) = x in y |] `shouldReturn`
                Let
                    objBnd
                    "x"
                    (Let "a" (mkNth 0 3) $
                     Let "b" (mkNth 1 3) $ Let "c" (mkNth 2 3) "y")
        it "removes destructuring from lambdas" $
            let objBnd = "d"
                mkNth = mkNth0 objBnd
             in runRemDestr [embedALang| \(a, b, c) -> y |] `shouldReturn`
                Lambda
                    objBnd
                    (Let "a" (mkNth 0 3) $
                     Let "b" (mkNth 1 3) $ Let "c" (mkNth 2 3) "y")
