module Ohua.Test.DFGraph where

import Protolude

import Data.Function
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap.Strict as IntMap

import Ohua.DFGraph
import Ohua.Types
import Ohua.Util


newtype OhuaGrGraph = OhuaGrGraph { unGr :: Gr QualifiedBinding OhuaGrEdgeLabel }
  deriving (Eq, Show)


data OhuaGrEdgeLabel = OhuaGrEdgeLabel
    { sourceIndex :: !Int
    , targetIndex :: !Int
    } deriving (Eq, Ord, Show)


-- To handle env args i generate one new node which is source for all env args.
-- The source index is the env arc number
toFGLGraph :: OutGraph -> OhuaGrGraph
toFGLGraph (OutGraph ops arcs _) = OhuaGrGraph $ mkGraph grNodes grEdges
  where
    regularNodes = map (\(Operator oid type_) -> (unwrap oid, type_)) ops

    envId = succ $ maximum $ map fst regularNodes -- one fresh id for an env node

    grNodes = (envId, "ohua.internal/env") : regularNodes

    grEdges = map arcToEdge arcs

    arcToEdge (Arc t s) = (sourceOp, unwrap $ operator t, OhuaGrEdgeLabel sourceIdx (index t))
      where
        (sourceOp, sourceIdx) = case s of
            LocalSource (Target op idx) -> (unwrap op, idx)
            EnvSource e                 -> (envId, unwrap e)


isIsomorphic :: (Eq a, Ord b) => Gr a b -> Gr a b -> Bool
isIsomorphic gr1 gr2 = isJust $ isomorphicMapping gr1 gr2

isomorphicMapping :: (Eq a, Ord b) => Gr a b -> Gr a b -> Maybe IsoMap
isomorphicMapping g1 g2 = either (const Nothing) Just $ matchGraph g1 g2


type IsoMap = IntMap.IntMap Int

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f a0 a1 | ((>=) `on` f) a0 a1 = a0
              | otherwise = a1

type IsoFailData = (IsoMap, Maybe (Int, Int))

plusEither :: Either IsoFailData b -> Either IsoFailData b -> Either IsoFailData b
plusEither (Left a) (Left a2) = Left $ maxOn (IntMap.size . fst) a a2
plusEither r@(Right _) _      = r
plusEither _ b                = b

emptyEither :: Either IsoFailData b
emptyEither = Left (mempty, Nothing)

sumEither :: Foldable f => f (Either IsoFailData b) -> Either IsoFailData b
sumEither = foldl' plusEither emptyEither

mapsEither :: Foldable f => (a -> Either IsoFailData b) -> f a -> Either IsoFailData b
mapsEither f = foldl' (\a b -> plusEither a (f b)) emptyEither

matchGraph :: (Eq a, Ord b) => Gr a b -> Gr a b -> Either IsoFailData IsoMap
matchGraph gr1 gr2 | order gr1 == 0 && order gr2 == 0 = Right mempty
matchGraph gr1 gr2 = go (nodes gr1) [] [] mempty
  where
    go :: [Int] -> [Int] -> [Int] -> IsoMap -> Either IsoFailData IsoMap
    go rest !gr1Selected !gr2Selected !mapping =
        if gr1Subgr == rename mapping (subgraph gr2Selected gr2) then
            descend rest
        else
            failMatch
      where
        gr1Subgr = subgraph gr1Selected gr1
        descend [] | gr1Subgr == gr1 && order gr2 == order gr1Subgr = Right mapping
                   | otherwise = failMatch
        descend (x:xs) = selectX `mapsEither` nodes gr2
          where selectX k = go xs (x:gr1Selected) (k:gr2Selected) $ IntMap.insert k x mapping

        failMatch = Left (lastMapping, lastSelects)

        lastSelects = case (gr1Selected, gr2Selected) of
            (x:_, k:_) -> Just (k, x)
            _          -> Nothing

        lastMapping = maybe id (IntMap.delete . fst) lastSelects mapping

    rename mapping gr = mkGraph ns es
      where
        ns = first newName <$> labNodes gr
        es = map (\(a, b, c) -> (newName a, newName b, c)) (labEdges gr)
        newName node = fromMaybe (panicS $ "Invariant broken: missing mapping for node " <> show node) $ IntMap.lookup node mapping
