{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances #-}
module DFLowering where

import           Control.Arrow
import           Control.Monad.Except
import           Data.Foldable
import           Data.Function
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.List
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import           Data.String
import           Debug.Trace
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types
import           Test.Hspec


newtype OhuaGrGraph = OhuaGrGraph { unGr :: Gr FnName OhuaGrEdgeLabel } deriving Eq


data OhuaGrEdgeLabel = OhuaGrEdgeLabel
    { sourceIndex :: !Int
    , targetIndex :: !Int
    } deriving (Eq, Show, Ord)

instance Show DFExpr where
    show (DFExpr e v) = intercalate "\n" (map show (toList e)) ++ "\n" ++ show v
deriving instance Show LetExpr
deriving instance Show DFVar
deriving instance Show DFFnRef

instance Show OhuaGrGraph where
    show = prettify . unGr

instance IsString DFFnRef where fromString = EmbedSf . fromString
instance Num ResolvedSymbol where fromInteger = Env . fromInteger
instance Num Expression where fromInteger = Var . fromInteger

-- To handle env args i generate one new node which is source for all env args.
-- The source index is the env arc number
toFGLGraph :: OutGraph -> OhuaGrGraph
toFGLGraph (OutGraph ops arcs) = OhuaGrGraph $ mkGraph nodes edges
  where
    regularNodes = map (\(Operator id type_) -> (unFnId id, type_)) ops

    envId = succ $ maximum $ map fst regularNodes -- one fresh id for an env node

    nodes = (envId, "com.ohua.internal/env") : regularNodes

    edges = map toEdge arcs

    toEdge (Arc t s) = (sourceOp, unFnId $ operator t, OhuaGrEdgeLabel sourceIdx (index t))
      where
        (sourceOp, sourceIdx) = case s of
            LocalSource (Target op idx) -> (unFnId op, idx)
            EnvSource e                 -> (envId, unwrapHostExpr e)


shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

traceGr :: OhuaGrGraph -> OhuaGrGraph
traceGr g = trace (prettify $ unGr g) g


runLowering :: Expression -> IO DFExpr
runLowering = runOhuaT (fmap (either error id) . runExceptT . lowerALang)


shouldLowerTo :: Expression -> DFExpr -> Expectation
shouldLowerTo input expected = do
    gr1 <- fmap (toFGLGraph . toGraph) (runLowering input)
    let gr2 = toFGLGraph $ toGraph expected
    unless (gr1 `matches` gr2) $ expectationFailure (show gr1 ++ "\nis not isomorph to\n\n" ++ show gr2)
  where matches = isIsomorphic `on` unGr


lowerAndValidate :: Expression -> DFExpr -> String -> Spec
lowerAndValidate sourceExpr targetExpr statementType = do
    it ("correctly lowers an " ++ statementType ++ " statement") $
        sourceExpr `shouldLowerTo` targetExpr


smapLowering :: Spec
smapLowering = describe "lowering smap constructs" $ do
    let sourceExpr =
            Let "coll" ("com.ohua.lang/id" `Apply` 0) $
            Let "x" ("com.ohua.lang/smap" `Apply` Lambda "y" (Let "z" ("some.module/inc" `Apply` "y") "z") `Apply` "coll")
            "x"
    let targetExpr = DFExpr
            [ LetExpr 0 "coll" "com.ohua.lang/id" [0] Nothing
            , LetExpr 4 "a" "com.ohua.lang/size" ["coll"] Nothing
            , LetExpr 5 "b" (DFFunction "com.ohua.lang/one-to-n") ["a", "coll"] Nothing
            , LetExpr 1 "y" (DFFunction "com.ohua.lang/smap-fun") ["b"] Nothing
            , LetExpr 2 "z" "some.module/inc" ["y"] Nothing
            , LetExpr 6 "c" (DFFunction "com.ohua.lang/one-to-n") ["a", "a"] Nothing
            , LetExpr 3 "x" (DFFunction "com.ohua.lang/collect") ["c", "z"] Nothing
            ]
            "x"
    lowerAndValidate sourceExpr targetExpr "smap"

smapSpec :: Spec
smapSpec = smapLowering

ifLowering :: Spec
ifLowering = describe "lowering conditionals" $ do
    let sourceExpr =
          Let "a" ("com.ohua.lang/id" `Apply` 0) $
          Let "b" ("com.ohua.lang/id" `Apply` 1) $
          Let "c" ("com.ohua.lang/id" `Apply` 2) $
          Let "z" (Apply (Apply (Apply "com.ohua.lang/if" "c")
                                (Lambda "then" (Let "p" (Apply "some-ns/+" "a")
                                                    (Let "f" (Apply "p" "b") "f"))))
                         (Lambda "else" (Let "m" (Apply "some-ns/-" "a")
                                             (Let "f" (Apply "m" "b") "f"))))
          "z"
    let targetExpr = DFExpr
          [ LetExpr 0 "a" "com.ohua.lang/id" [0] Nothing
          , LetExpr 1 "b" "com.ohua.lang/id" [1] Nothing
          , LetExpr 2 "c" "com.ohua.lang/id" [2] Nothing
          , LetExpr 3 "s" (DFFunction "com.ohua.lang/ifThenElse") ["c"] Nothing
          , LetExpr 4 "d" "some-ns/+" ["a", "b"] Nothing
          , LetExpr 5 "e" "some-ns/-" ["a", "b"] Nothing
          , LetExpr 6 "z" (DFFunction "com.ohua.lang/switch") ["s", "d", "e"] Nothing
          ]
          "z"

    lowerAndValidate (traceShowId sourceExpr) targetExpr "if"


generalLowering :: Spec
generalLowering = do
    describe "lowering a stateful function" $ do
        it "lowers a function with one argument" $
            Let "a" ("com.ohua.lang/id" `Apply` 0) (Let "x" ("some/function" `Apply` "a") "x")
            `shouldLowerTo`
            DFExpr
                [ LetExpr 1 "a" "com.ohua.lang/id" [0] Nothing
                , LetExpr 0 "x" "some/function" ["a"] Nothing
                ] "x"
        it "lowers a function with one env argument" $
            Let "x" ("some/function" `Apply` 0) "x"
            `shouldLowerTo`
            DFExpr [ LetExpr 0 "x" "some/function" [0] Nothing ] "x"
        it "lowers a function with no arguments" $
            Let "x" "some/function" "x"
            `shouldLowerTo`
            DFExpr [ LetExpr 0 "x" "some/function" [] Nothing ] "x"


ifSpec :: Spec
ifSpec = ifLowering


instance IsString a => IsString (Maybe a) where fromString = Just . fromString


seqSpec :: Spec
seqSpec = do
    describe "seq lowering" $ do
        it "lowers a simple seq" $
            (   Let "y" ("com.ohua.lang/id" `Apply` 0) $
                Let "x" ("com.ohua.lang/seq" `Apply` "y" `Apply` Lambda "_" (Let "p" "some/function" "p"))
                    "x"
            )
            `shouldLowerTo`
            DFExpr
                [ LetExpr 1 "y" (EmbedSf "com.ohua.lang/id") [0] Nothing
                , LetExpr 0 "x" (EmbedSf "some/function") [] "y"
                ]
                "x"

-- (let [a (fn [i] (let [p (math/- i 10)]
--                       (ohua.lang/if (math/< p 0)
--                                     p
--                                     (a p))))]
--       (a 95))
recurSpec :: Spec
recurSpec = do
    describe "recur lowering" $ do
         it "lowers a simple recursion" $
            (   Let (Recursive (Binding "a"))
                        (Lambda "i"
                                (Let "p" (("math/-" `Apply` "i") `Apply` 10)
                                     (Let "x" (("math/<" `Apply` "p") `Apply` 0)
                                          (Let "r" (("ohua.lang/recur" `Apply` "a") `Apply` "p")
                                               (Let "c" ((("ohua.lang/if" `Apply` "x") `Apply` "p") `Apply` "r")
                                                    "c")))))
                     (Let "y" ("a" `Apply` 95)
                          "y")
            )
            `shouldLowerTo`
            DFExpr
                [ LetExpr 0 "a" (EmbedSf "ohua.lang/id") [DFEnvVar 95] Nothing
                , LetExpr 1 "p" (EmbedSf "math/-") [DFVar "i", DFEnvVar 10] Nothing
                , LetExpr 2 "c" (EmbedSf "math/<") [DFVar "p", DFEnvVar 0] Nothing
                , LetExpr 3 ["t", "f"] (DFFunction "ohua.lang/if") [DFVar "c"] Nothing
                , LetExpr 4 "tr" (EmbedSf "ohua.lang/id") [DFVar "p"] $ Just "t"
                , LetExpr 5 "recur-array" (EmbedSf "ohua.lang/array") [DFVar "p"] $ Just "f"
                , LetExpr 6 "in-algo-array" (EmbedSf "ohua.lang/array") [DFVar "a"] Nothing
                , LetExpr 7 "r" (DFFunction "ohua.lang/recur") [DFVar "c", DFVar "algo-in-array", DFVar "recur-array"] Nothing
                ]
                "tr"

isIsomorphic :: (Eq a, Ord b) => Gr a b -> Gr a b -> Bool
isIsomorphic gr1 gr2 = isJust $ isomorphicMapping gr1 gr2

isomorphicMapping :: (Eq a, Ord b) => Gr a b -> Gr a b -> Maybe (Map.Map Int Int)
isomorphicMapping gr1 gr2 | order gr1 /= order gr2 || size gr1 /= size gr2 = Nothing
isomorphicMapping gr1 gr2 = go (nodes gr1) [] [] mempty
  where
    go rest !gr1Selected !gr2Selected !mapping | gr1Subgr == rename mapping (subgraph gr2Selected gr2) = descend rest
      where
        gr1Subgr = subgraph gr1Selected gr1
        descend [] | gr1Subgr == gr1 = Just mapping
        descend (x:xs) = msum $ map selectX (nodes gr2)
          where selectX k = go xs (x:gr1Selected) (k:gr2Selected) (Map.insert k x mapping)
    go _ _ _ _ = Nothing

    rename mapping gr = mkGraph ns es
      where
        ns = map (first newName) (labNodes gr)
        es = map (\(a, b, c) -> (newName a, newName b, c)) (labEdges gr)
        newName node = fromMaybe (error $ "Invariant broken: missing mapping for node " ++ show node) $ Map.lookup node mapping
