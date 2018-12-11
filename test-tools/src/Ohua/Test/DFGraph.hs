module Ohua.Test.DFGraph where

import Ohua.Prelude

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap.Strict as IntMap

import Ohua.DFGraph

newtype OhuaGrGraph = OhuaGrGraph
    { unGr :: Gr QualifiedBinding OhuaGrEdgeLabel
    } deriving (Eq, Show)

data OhuaGrEdgeLabel = OhuaGrEdgeLabel
    { sourceIndex :: !Int
    , targetIndex :: !Int
    } deriving (Eq, Ord, Show)

-- TODO support for compound arcs
-- To handle env args i generate one new node which is source for all env args.
-- The source index is the env arc number
toFGLGraph :: OutGraph -> OhuaGrGraph
toFGLGraph (OutGraph ops (Arcs arcs _ _) _) =
    OhuaGrGraph $ mkGraph grNodes grEdges
  where
    regularNodes = map (\(Operator oid type_ _) -> (unwrap oid, type_)) ops
    envId = succ $ maximum $ map fst regularNodes -- one fresh id for an env node
    numLitId = succ envId
    grNodes = (envId, "ohua.internal/env") : regularNodes
    grEdges = map arcToEdge arcs
    arcToEdge (Arc t s) =
        (sourceOp, unwrap $ operator t, OhuaGrEdgeLabel sourceIdx (index t))
      where
        (sourceOp, sourceIdx) =
            case s of
                LocalSource (Target op idx) -> (unwrap op, idx)
                EnvSource e ->
                    case e of
                        EnvRefLit he -> (envId, unwrap he)
                        NumericLit n -> (numLitId, fromInteger n)
                        _ -> error "Unsupported literal for this function"

isIsomorphic :: (Eq a, Ord b) => Gr a b -> Gr a b -> Bool
isIsomorphic gr1 gr2 = isJust $ isomorphicMapping gr1 gr2

isomorphicMapping :: (Eq a, Ord b) => Gr a b -> Gr a b -> Maybe IsoMap
isomorphicMapping g1 g2 = either (const Nothing) Just $ matchGraph g1 g2

type IsoMap = IntMap.IntMap Int

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f a0 a1
    | ((>=) `on` f) a0 a1 = a0
    | otherwise = a1

type IsoFailData = (IsoMap, Maybe (Int, Int))

plusEither ::
       Either IsoFailData b -> Either IsoFailData b -> Either IsoFailData b
plusEither (Left a) (Left a2) = Left $ maxOn (IntMap.size . fst) a a2
plusEither r@(Right _) _ = r
plusEither _ b = b

emptyEither :: Either IsoFailData b
emptyEither = Left (mempty, Nothing)

sumEither ::
       (Container c, Element c ~ Either IsoFailData b)
    => c
    -> Either IsoFailData b
sumEither = foldl' plusEither emptyEither

mapsEither ::
       Container c
    => (Element c -> Either IsoFailData b)
    -> c
    -> Either IsoFailData b
mapsEither f = foldl' (\a b -> plusEither a (f b)) emptyEither

matchGraph :: (Eq a, Ord b) => Gr a b -> Gr a b -> Either IsoFailData IsoMap
matchGraph gr1 gr2
    | order gr1 == 0 && order gr2 == 0 = Right mempty
matchGraph gr1 gr2 = go (nodes gr1) [] [] mempty
  where
    go :: [Int] -> [Int] -> [Int] -> IsoMap -> Either IsoFailData IsoMap
    go rest !gr1Selected !gr2Selected !mapping =
        if gr1Subgr == rename mapping (subgraph gr2Selected gr2)
            then descend rest
            else failMatch
      where
        gr1Subgr = subgraph gr1Selected gr1
        descend []
            | gr1Subgr == gr1 && order gr2 == order gr1Subgr = Right mapping
            | otherwise = failMatch
        descend (x:xs) = selectX `mapsEither` nodes gr2
          where
            selectX k =
                go xs (x : gr1Selected) (k : gr2Selected) $
                IntMap.insert k x mapping
        failMatch = Left (lastMapping, lastSelects)
        lastSelects =
            case (gr1Selected, gr2Selected) of
                (x:_, k:_) -> Just (k, x)
                _ -> Nothing
        lastMapping = maybe id (IntMap.delete . fst) lastSelects mapping
    rename mapping gr = mkGraph ns es
      where
        ns = first newName <$> labNodes gr
        es = map (\(a, b, c) -> (newName a, newName b, c)) (labEdges gr)
        newName node =
            fromMaybe
                (error $
                 "Invariant broken: missing mapping for node " <> show node) $
            IntMap.lookup node mapping
