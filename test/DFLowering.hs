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


smapLowering :: Spec
smapLowering = describe "lowering smap constructs" $ do
    let sourceExpr =
            Let "coll" ("com.ohua.lang/id" `Apply` (Var $ Env 0)) $
            Let "x" ("com.ohua.lang/smap" `Apply` (Lambda "y" (Let "z" (Apply "some.module/inc" "y") "z")) `Apply` "coll")
            "x"
    let targetExpr = DFExpr
            [ LetExpr 0 "coll" (EmbedSf "com.ohua.lang/id") [DFEnvVar (HostExpr 0)] Nothing
            , LetExpr 1 "y" (DFFunction "com.ohua.lang/smap-fun") [DFVar "coll"] Nothing
            , LetExpr 2 "z" (EmbedSf "some.module/inc") [DFVar "y"] Nothing
            , LetExpr 3 "x" (DFFunction "com.ohua.lang/collect") [DFVar "z"] Nothing
            ]
            "x"
    let runLowering = runOhuaC (fmap (either error id) . runExceptT . lowerALang)
    let shouldLowerTo :: Expression -> DFExpr -> Expectation
        shouldLowerTo input expected =
            fmap (toFGLGraph . toGraph) (runLowering input) `shouldSatisfyRet` (isIsomorphic (unGr $ toFGLGraph $ toGraph expected) . unGr)
    it "correctly lowers an smap statment" $
        sourceExpr `shouldLowerTo` targetExpr


spec :: Spec
spec = do
    smapLowering

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
