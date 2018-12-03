{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

module DFLowering where

import Ohua.Prelude

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Test.Hspec

import Ohua.ALang.Lang
import Ohua.ALang.Passes
import qualified Ohua.ALang.Refs as ALangRefs
import Ohua.DFGraph
import Ohua.DFLang.Lang
import Ohua.DFLang.PPrint
import Ohua.DFLang.Passes
import qualified Ohua.DFLang.Refs as Refs
import Ohua.Test (embedALang, embedDFLang)
import Ohua.Test.DFGraph

shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

runLowering :: Expression -> IO DFExpr
runLowering =
    fmap (either error identity) .
    runSilentLoggingT .
    runFromExpr
        def
        (lowerALang <=<
         normalize <=< Ohua.ALang.Passes.runCorePasses <=< normalize)

-- | IMPORTANT: Both source and target expression must be in SSA form
shouldLowerTo :: Expression -> DFExpr -> Expectation
shouldLowerTo input expected = do
    lowered <- runLowering input
    traceM $ "expected:\n" <> (show $ prettyDFExpr expected)
    traceM $ "got:\n" <> (show $ prettyDFExpr lowered)
    lowered `shouldBe` expected
    -- let gr1 = (toFGLGraph . toGraph) lowered
    -- let gr2 = toFGLGraph $ toGraph expected
    -- (matchAndReport `on` unGr) gr1 gr2

lowerAndValidate :: Expression -> DFExpr -> String -> Spec
lowerAndValidate sourceExpr targetExpr statementType = do
    it ("correctly lowers an " ++ statementType ++ " statement") $
        sourceExpr `shouldLowerTo` targetExpr

smapLowering :: Spec
smapLowering =
    describe "lowering smap constructs" $ do
        let sourceExpr
                -- Let "coll" (sf ALangRefs.id `Apply` 0) $
                -- Let
                --     "x"
                --     (sf ALangRefs.smap `Apply`
                --      Lambda "y" (Let "z" ("some.module/inc" `Apply` "y") "z") `Apply`
                --      "coll")
                --     "x"
             =
                [embedALang|
                        let coll = ohua.lang/id 0 in
                          let x = ohua.lang/smap
                                    (\y -> let z = some.module/inc y in z)
                                    coll in
                            x
                               |]
        let targetExpr
                -- DFExpr
                --     [ LetExpr 0 "coll" Refs.id [0] Nothing
                --     , LetExpr 4 "a" Refs.size ["coll"] Nothing
                --     , LetExpr 5 "b" Refs.oneToN ["a", "coll"] Nothing
                --     , LetExpr 1 "y" Refs.smapFun ["b"] Nothing
                --     , LetExpr 2 "z" "some.module/inc" ["y"] Nothing
                --     , LetExpr 6 "c" Refs.oneToN ["a", "a"] Nothing
                --     , LetExpr 3 "x" Refs.collect ["c", "z"] Nothing
                --     ]
                --     "x"
             =
                [embedDFLang|
                  let (coll) = ohua.lang/id<0> (0) in
                  let (a) = ohua.lang/size<4> (coll) in
                  let (b) = ohua.lang/oneToN<5> (a,coll) in
                  let (y) = dataflow ohua.lang/smapFun<1> (b) in
                  let (z) = some.module/inc<2> (y) in
                  let (c) = ohua.lang/oneToN<6> (a,a) in
                  let (x) = dataflow ohua.lang/collect<3> (c,z) in
                    x
                |]
        lowerAndValidate sourceExpr targetExpr "smap"

smapSpec :: Spec
smapSpec = smapLowering

ifLowering :: Spec
ifLowering =
    describe "lowering conditionals" $ do
        let sourceExpr
                -- Let "a" (sf ALangRefs.id `Apply` 0) $
                -- Let "b" (sf ALangRefs.id `Apply` 1) $
                -- Let "c" (sf ALangRefs.id `Apply` 2) $
                -- Let
                --     "z"
                --     (Apply
                --          (Apply
                --               (Apply (sf ALangRefs.ifThenElse) "c")
                --               (Lambda
                --                    "then"
                --                    (Let "f"
                --                         (Apply (Apply "some-ns/+" "a") "b")
                --                         "f")))
                --          (Lambda
                --               "else"
                --               (Let "f0" (Apply (Apply "some-ns/-" "a") "b") "f0")))
                --     "z"
             =
                [embedALang|
                  let a = ohua.lang/id 0 in
                  let b = ohua.lang/id 1 in
                  let c = ohua.lang/id 2 in
                  let z = ohua.lang/if
                            c
                           (\() -> let f = someNs/plus a b in f)
                           (\() -> let f0 = someNs/minus a b in f0) in
                    z
                |]
        let targetExpr
                -- DFExpr
                --     [ LetExpr 0 "a" Refs.id [0] Nothing
                --     , LetExpr 1 "b" Refs.id [1] Nothing
                --     , LetExpr 2 "c" Refs.id [2] Nothing
                --     , LetExpr 3 ["true", "false"] Refs.bool ["c"] Nothing
                --     , LetExpr 7 ["a0", "b0"] Refs.scope ["a", "b"] (Just "true")
                --     , LetExpr
                --           8
                --           ["a1", "b1"]
                --           Refs.scope
                --           ["a", "b"]
                --           (Just "false")
                --     , LetExpr 4 "d" "some-ns/+" ["a0", "b0"] Nothing
                --     , LetExpr 5 "e" "some-ns/-" ["a1", "b1"] Nothing
                --     , LetExpr 6 "z" Refs.select ["true", "d", "e"] Nothing
                --     ]
                --     "z"
             =
                [embedDFLang|
                  let (a) = ohua.lang/id<1> (0)  in
                  let (b) = ohua.lang/id<2> (1)  in
                  let (c) = ohua.lang/id<3> (2)  in
                  let (ctrls_0) = dataflow ohua.lang/ifFun<4> (c)  in
                  let (ctrlTrue_1) = ohua.lang/nth<5> (0, ctrls_0)  in
                  let (ctrlFalse_1) = ohua.lang/nth<6> (1, ctrls_0)  in
                  let (ctrl_0) = dataflow ohua.lang/ctrl<7> (ctrlTrue_0, a, b, ())  in
                  let (a_0) = ohua.lang/nth<8> (0, ctrl_0)  in
                  let (b_0) = ohua.lang/nth<9> (1, ctrl_0)  in
                  let (unitVar_0) = ohua.lang/nth<10> (2, ctrl_0)  in
                  let (trueResult_0) = someNs/plus<11> (a_0, b_0)  in
                  let (ctrl_1) = dataflow ohua.lang/ctrl<12> (ctrlFalse_0, a, b, ())  in
                  let (a_1) = ohua.lang/nth<13> (0, ctrl_1)  in
                  let (b_1) = ohua.lang/nth<14> (1, ctrl_1)  in
                  let (unitVar_1) = ohua.lang/nth<15> (2, ctrl_1)  in
                  let (falseResult_0) = someNs/minus<16> (a_1, b_1)  in
                  let (z) = dataflow ohua.lang/select<17> (c, trueResult_0, falseResult_0)  in
                    z
                |]
        lowerAndValidate sourceExpr targetExpr "if"

generalLowering :: Spec
generalLowering = do
    describe "lowering a stateful function" $ do
        it "lowers a function with one argument" $
            -- Let
            --     "a"
            --     (sf ALangRefs.id `Apply` 0)
            --     (Let "x" ("some/function" `Apply` "a") "x")
            [embedALang|
              let a = ohua.lang/id 0 in
              let x = some/function a in
                x
              |] `shouldLowerTo`
            -- DFExpr
            --     [ LetExpr 1 "a" Refs.id [0] Nothing
            --     , LetExpr 0 "x" "some/function" ["a"] Nothing
            --     ]
            --     "x"
            [embedDFLang|
              let (a) = ohua.lang/id<1> (0) in
              let (x) = some/function<2> (a) in
                x
            |]
        it "lowers a function with one env argument" $
            -- Let
            --     "x"
            --     ("some/function" `Apply` 0)
            --     "x"
            [embedALang| let x = some/function 0 in x |] `shouldLowerTo`
            -- DFExpr [LetExpr 0 "x" "some/function" [0] Nothing] "x"
            [embedDFLang| let (x) = some/function<1> (0) in x |]
        -- left out for the moment one we merge the unit stuff into the core we can add it back
        -- it "lowers a function with no arguments" $
        --     Let "x" "some/function" "x"
        --     `shouldLowerTo`
        --     DFExpr [ LetExpr 0 "x" "some/function" [] Nothing ] "x"

ifSpec :: Spec
ifSpec = ifLowering

instance IsString a => IsString (Maybe a) where
    fromString = Just . fromString

seqSpec :: Spec
seqSpec = do
    describe "seq lowering" $ do
        it "lowers a simple seq" $
            -- (Let "y" (sf ALangRefs.id `Apply` 0) $
            --  Let
            --      "x"
            --      (sf ALangRefs.seq `Apply` "y" `Apply`
            --       Lambda
            --           "_"
            --           (Let "p"
            --                ("some/function" `Apply` (Var (Env (1 :: HostExpr))))
            --                "p"))
            --      "x")
            [embedALang|
              let y = ohua.lang/id 0 in
              let x = ohua.lang/seq y (let p = some/function 1 in p) in
                x
            |] `shouldLowerTo`
            -- DFExpr
            --     [ LetExpr 1 "y" Refs.id [0] Nothing
            --     , LetExpr 2 "y0" Refs.seq ["y"] Nothing
            --     , LetExpr 0 "x" (EmbedSf "some/function") [1] "y0"
            --     ]
            --     "x"
            [embedDFLang|
              let (y) = ohua.lang/id<1> (0) in
              let (y0) = ohua.lang/seq<2> (y) in
              let (x) = some/function<0> (1) [y0] in
                x
            |]

matchAndReport :: (Eq a, Ord b, Show a, Show b) => Gr a b -> Gr a b -> IO ()
matchAndReport gr1 gr2
      -- TODO add a check here to verify that all nodes have unique function IDs
--matchAndReport g1 g2 =
--    let gr1 = trace ("Graph #1: " ++ show g1) g1
--        gr2 = trace ("Graph #2: " ++ show g2) g2 in
 =
    case matchGraph gr1 gr2 of
        Right _ -> return ()
        Left (largest, keys0) ->
            let selectedGr1Nodes = IntMap.elems largest
                selectedGr2Nodes = IntMap.keys largest
                unselectedGr1Nodes =
                    filter
                        (not .
                         flip IntSet.member (IntSet.fromList selectedGr1Nodes) .
                         fst)
                        (labNodes gr1)
                unselectedGr2Nodes =
                    filter
                        (not . flip IntSet.member (IntMap.keysSet largest) . fst)
                        (labNodes gr2)
             in expectationFailure $
                toString $
                unlines
                    [ "Graphs weren't isomorphic."
                    , "The largest match was between"
                    , ""
                    , prettify0 (subgraph selectedGr1Nodes gr1)
                    , ""
                    , "and"
                    , ""
                    , prettify0 (subgraph selectedGr2Nodes gr2)
                    , ""
                    , "I could not match the nodes"
                    , ""
                    , show $ unselectedGr1Nodes
                    , ""
                    , "with"
                    , ""
                    , show $ unselectedGr2Nodes
                    , case keys0 of
                          Nothing -> ""
                          Just (_, x) ->
                              unlines
                                  [ ""
                                  , "I failed when trying to match"
                                  , show $
                                    filter ((== x) . fst) unselectedGr1Nodes
                                  , "with the edges:"
                                  , show $
                                    filter
                                        (\(a, b, _) -> a == x || b == x)
                                        (labEdges gr1)
                                  , "With any of: "
                                  , show unselectedGr2Nodes
                                  ]
                    ]
  where
    prettify0 = toText . prettify
