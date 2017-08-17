{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module DFLowering where

import           Control.Arrow
import           Control.Monad.Except
import           Data.Function
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import           Debug.Trace
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types
import           Test.Hspec
import Data.String


newtype OhuaGrGraph = OhuaGrGraph { unGr :: Gr FnName (Int, Int) } deriving Eq

instance Show OhuaGrGraph where
    show = prettify . unGr

-- To handle env args i generate one new node which is source for all env args.
-- The source index is the env arc number
toFGLGraph :: OutGraph -> OhuaGrGraph
toFGLGraph (OutGraph ops arcs) = OhuaGrGraph $ mkGraph nodes edges
  where
    regularNodes = map (\(Operator id type_) -> (unFnId id, type_)) ops

    envId = succ $ maximum $ map fst regularNodes -- one fresh id for an env node

    nodes = (envId, "com.ohua.internal/env") : regularNodes

    edges = map toEdge arcs

    toEdge (Arc s t) = (unFnId $ operator s, unFnId $ operator t, (index s, index t))
    toEdge (EnvArc t a) = (envId, unFnId $ operator t, (unwrapHostExpr a, index t))



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
            Let "coll" ("com.ohua.lang/id" `Apply` Var (Env 0)) $
            Let "x" ("com.ohua.lang/smap" `Apply` Lambda "y" (Let "z" (Apply "some.module/inc" "y") "z") `Apply` "coll")
            "x"
    let targetExpr = DFExpr
            [ LetExpr 0 "coll" (EmbedSf "com.ohua.lang/id") [DFEnvVar (HostExpr 0)] Nothing
            , LetExpr 1 "y" (DFFunction "com.ohua.lang/smap-fun") [DFVar "coll"] Nothing
            , LetExpr 2 "z" (EmbedSf "some.module/inc") [DFVar "y"] Nothing
            , LetExpr 3 "x" (DFFunction "com.ohua.lang/collect") [DFVar "z"] Nothing
            ]
            "x"
    lowerAndValidate sourceExpr targetExpr "smap"

smapSpec :: Spec
smapSpec = smapLowering

ifLowering :: Spec
ifLowering = describe "lowering conditionals" $ do
    let sourceExpr =
          Let "a" ("com.ohua.lang/id" `Apply` (Var $ Env 0)) $
          Let "b" ("com.ohua.lang/id" `Apply` (Var $ Env 1)) $
          Let "c" ("com.ohua.lang/id" `Apply` (Var $ Env 2)) $
          Let "z" (Apply (Apply (Apply "com.ohua.lang/if" "c")
                                (Lambda "then" (Let "p" (Apply "some-ns/+" "a")
                                                    (Let "f" (Apply "p" "b") "f"))))
                         (Lambda "else" (Let "m" (Apply "some-ns/-" "a")
                                             (Let "f" (Apply "m" "b") "f"))))
          "z"
    let targetExpr = DFExpr
          [ LetExpr 0 "a" (EmbedSf "com.ohua.lang/id") [0] Nothing
          , LetExpr 1 "b" (EmbedSf "com.ohua.lang/id") [1] Nothing
          , LetExpr 2 "c" (EmbedSf "com.ohua.lang/id") [2] Nothing
          , LetExpr 3 "s" (DFFunction "com.ohua.lang/ifThenElse") ["c"] Nothing
          , LetExpr 4 "d" (EmbedSf "some-ns/+") ["a", "b"] Nothing
          , LetExpr 5 "e" (EmbedSf "some-ns/-") ["a", "b"] Nothing
          , LetExpr 6 "z" (DFFunction "com.ohua.lang/switch") ["s", "d", "e"] Nothing
          ]
          "z"

    lowerAndValidate (traceShowId sourceExpr) targetExpr "if"


generalLowering :: Spec
generalLowering = do
    describe "lowering a stateful function" $ do
        it "lowers a function with one argument" $
            Let "a" ("com.ohua.lang/id" `Apply` Var (Env 0)) (Let "x" ("some/function" `Apply` "a") "x")
            `shouldLowerTo`
            DFExpr [ LetExpr 1 "a" (EmbedSf "com.ohua.lang/id") [0] Nothing, LetExpr 0 "x" (EmbedSf "some/function") ["a"] Nothing ] "x"
        it "lowers a function with one env argument" $
            Let "x" ("some/function" `Apply` Var (Env 0)) "x"
            `shouldLowerTo`
            DFExpr [ LetExpr 0 "x" (EmbedSf "some/function") [0] Nothing ] "x"
        it "lowers a function with no arguments" $
            Let "x" "some/function" "x"
            `shouldLowerTo`
            DFExpr [ LetExpr 0 "x" (EmbedSf "some/function") [] Nothing ] "x"


ifSpec :: Spec
ifSpec = ifLowering


instance IsString a => IsString (Maybe a) where fromString = Just . fromString


seqSpec :: Spec
seqSpec = do
    describe "seq lowering" $ do
        it "lowers a simple seq" $
            Let "y" ("com.ohua.lang/id" `Apply` Var (Env 0)) (Let "x" ("com.ohua.lang/seq" `Apply` "y" `Apply` Lambda "_" (Let "p" "some/function" "p")) "x")
            `shouldLowerTo`
            DFExpr [ LetExpr 1 "y" (EmbedSf "com.ohua.lang/id") [0] Nothing, LetExpr 0 "x" (EmbedSf "some/function") [] (Just "y"), LetExpr 2 "z" (EmbedSf "com.ohua.lang/id") ["x"] Nothing ] "x"


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
