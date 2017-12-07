{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples      #-}
module DFLowering where

import           Control.Arrow
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.IntSet                       as IntSet
import           Data.List
import           Data.Maybe
import           Data.String
import qualified Data.Text                         as T
import           Debug.Trace
import           Ohua.ALang.Lang
import qualified Ohua.ALang.Refs                   as ALangRefs
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Passes
import qualified Ohua.DFLang.Refs                  as Refs
import           Ohua.Monad
import           Ohua.Types
import qualified Ohua.Util.Str                     as Str
import           Test.Hspec


newtype OhuaGrGraph = OhuaGrGraph { unGr :: Gr QualifiedBinding OhuaGrEdgeLabel } deriving Eq


data OhuaGrEdgeLabel = OhuaGrEdgeLabel
    { sourceIndex :: !Int
    , targetIndex :: !Int
    } deriving (Eq, Show, Ord)

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
        (# sourceOp, sourceIdx #) = case s of
            LocalSource (Target op idx) -> (# unFnId op, idx #)
            EnvSource e                 -> (# envId, unwrapHostExpr e #)


shouldSatisfyRet :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyRet action predicate = action >>= (`shouldSatisfy` predicate)

traceGr :: OhuaGrGraph -> OhuaGrGraph
traceGr g = trace (prettify $ unGr g) g


runLowering :: Expression -> IO DFExpr
runLowering = fmap (either (error . Str.toString) id) . runSilentLoggingT . runFromExpr def lowerALang


-- | IMPORTANT: Both source and target expression must be in SSA form
shouldLowerTo :: Expression -> DFExpr -> Expectation
shouldLowerTo input expected = do
    gr1 <- fmap (toFGLGraph . toGraph) (runLowering input)
    let gr2 = toFGLGraph $ toGraph expected
    (matchAndReport `on` unGr) gr1 gr2


lowerAndValidate :: Expression -> DFExpr -> String -> Spec
lowerAndValidate sourceExpr targetExpr statementType = do
    it ("correctly lowers an " ++ statementType ++ " statement") $
        sourceExpr `shouldLowerTo` targetExpr


smapLowering :: Spec
smapLowering = describe "lowering smap constructs" $ do
    let sourceExpr =
            Let "coll" ("ohua.lang/id" `Apply` 0) $
            Let "x" ("ohua.lang/smap" `Apply` Lambda "y" (Let "z" ("some.module/inc" `Apply` "y") "z") `Apply` "coll")
            "x"
    let targetExpr = DFExpr
            [ LetExpr 0 "coll" "ohua.lang/id" [0] Nothing
            , LetExpr 4 "a" "ohua.lang/size" ["coll"] Nothing
            , LetExpr 5 "b" (DFFunction "ohua.lang/one-to-n") ["a", "coll"] Nothing
            , LetExpr 1 "y" (DFFunction "ohua.lang/smap-fun") ["b"] Nothing
            , LetExpr 2 "z" "some.module/inc" ["y"] Nothing
            , LetExpr 6 "c" (DFFunction "ohua.lang/one-to-n") ["a", "a"] Nothing
            , LetExpr 3 "x" (DFFunction "ohua.lang/collect") ["c", "z"] Nothing
            ]
            "x"
    lowerAndValidate sourceExpr targetExpr "smap"

smapSpec :: Spec
smapSpec = smapLowering

ifLowering :: Spec
ifLowering = describe "lowering conditionals" $ do
    let sourceExpr =
          Let "a" ("ohua.lang/id" `Apply` 0) $
          Let "b" ("ohua.lang/id" `Apply` 1) $
          Let "c" ("ohua.lang/id" `Apply` 2) $
          Let "z" (Apply (Apply (Apply "ohua.lang/if" "c")
                                (Lambda "then" (Let "f" (Apply (Apply "some-ns/+" "a") "b") "f")))
                                (Lambda "else" (Let "f0" (Apply (Apply "some-ns/-" "a") "b") "f0")))
          "z"
    let targetExpr = DFExpr
          [ LetExpr 0 "a" "ohua.lang/id" [0] Nothing
          , LetExpr 1 "b" "ohua.lang/id" [1] Nothing
          , LetExpr 2 "c" "ohua.lang/id" [2] Nothing
          , LetExpr 3 "s" (DFFunction "ohua.lang/ifThenElse") ["c"] Nothing
          , LetExpr 4 "d" "some-ns/+" ["a", "b"] Nothing
          , LetExpr 5 "e" "some-ns/-" ["a", "b"] Nothing
          , LetExpr 6 "z" (DFFunction "ohua.lang/switch") ["s", "d", "e"] Nothing
          ]
          "z"

    lowerAndValidate (traceShowId sourceExpr) targetExpr "if"


generalLowering :: Spec
generalLowering = do
    describe "lowering a stateful function" $ do
        it "lowers a function with one argument" $
            Let "a" ("ohua.lang/id" `Apply` 0) (Let "x" ("some/function" `Apply` "a") "x")
            `shouldLowerTo`
            DFExpr
                [ LetExpr 1 "a" "ohua.lang/id" [0] Nothing
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
            (   Let "y" ("ohua.lang/id" `Apply` 0) $
                Let "x" ("ohua.lang/seq" `Apply` "y" `Apply` Lambda "_" (Let "p" "some/function" "p"))
                    "x"
            )
            `shouldLowerTo`
            DFExpr
                [ LetExpr 1 "y" (EmbedSf "ohua.lang/id") [0] Nothing
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
                                          (Let "c" (Apply (Apply (Apply (Var $ Sf ALangRefs.ifThenElse Nothing) "x")
                                                                        (Lambda "then" (Let "t" (Apply (Var $ Sf ALangRefs.id Nothing) "p")
                                                                                            "t")))
                                                                        (Lambda "else" (Let "r" ((Var $ Sf ALangRefs.recur Nothing) `Apply` "p")
                                                                                            "r")))
                                                "c"))))
                     (Let "y" ("a" `Apply` 95)
                          "y")
            )
            `shouldLowerTo`
            DFExpr
                [ LetExpr 0 "i_0" Refs.id [DFEnvVar 95] Nothing -- this adapts the actual to the formal that goes into algo-in and then becomes "i"
                -- inside the lambda everything is left untouched.
                , LetExpr 1 "p" (EmbedSf "math/-") [DFVar "i", DFEnvVar 10] Nothing
                , LetExpr 2 "x" (EmbedSf "math/<") [DFVar "p", DFEnvVar 0] Nothing
                , LetExpr 3 ["then", "else"] Refs.ifThenElse [DFVar "x"] Nothing
                , LetExpr 4 ["p_0"] Refs.scope [DFVar "p"] $ Just "then"
                , LetExpr 5 "t" Refs.id [DFVar "p_0"] Nothing
                , LetExpr 6 ["p_1"] Refs.scope [DFVar "p"] $ Just "else"
                -- the two functions to gather the parameters for the call to recur
                , LetExpr 7 "recur-in_0" Refs.array [DFVar "p_1"] Nothing
                , LetExpr 8 "algo-in_0" Refs.array [DFVar "i_0"] Nothing
                -- note: recur produces finally the formal input vars of the lambda
                , LetExpr 9 ["i"] Refs.recur [DFVar "x", DFVar "algo-in_0", DFVar "recur-in_0"] Nothing
                , LetExpr 10 "y" Refs.id [DFVar "t"] Nothing -- this adapts the output formal to the output actual
                ]
                "y"


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
matchGraph gr1 gr2 = go (nodes gr1) [] [] mempty
  where
    go :: [Int] -> [Int] -> [Int] -> IsoMap -> Either IsoFailData IsoMap
    go rest !gr1Selected !gr2Selected !mapping =
        if gr1Subgr == rename mapping (subgraph gr2Selected gr2) then
            descend rest
        else
            fail
      where
        gr1Subgr = subgraph gr1Selected gr1
        descend [] | gr1Subgr == gr1 && order gr2 == order gr1Subgr = Right mapping
                   | otherwise = fail
        descend (x:xs) = selectX `mapsEither` nodes gr2
          where selectX k = go xs (x:gr1Selected) (k:gr2Selected) $ IntMap.insert k x mapping

        fail = Left (lastMapping, lastSelects)

        lastSelects = case (gr1Selected, gr2Selected) of
            (x:_, k:_) -> Just (k, x)
            _          -> Nothing

        lastMapping = maybe id (IntMap.delete . fst) lastSelects mapping

    rename mapping gr = mkGraph ns es
      where
        ns = first newName <$> labNodes gr
        es = map (\(a, b, c) -> (newName a, newName b, c)) (labEdges gr)
        newName node = fromMaybe (error $ "Invariant broken: missing mapping for node " ++ show node) $ IntMap.lookup node mapping
matchGraph gr1 gr2 | order gr1 == 0 && order gr2 == 0 = Right mempty
matchGraph _ _ = emptyEither

matchAndReport :: (Eq a, Ord b, Show a, Show b) => Gr a b -> Gr a b -> Expectation
matchAndReport gr1 gr2 =
      -- TODO add a check here to verify that all nodes have unique function IDs
--matchAndReport g1 g2 =
--    let gr1 = trace ("Graph #1: " ++ show g1) g1
--        gr2 = trace ("Graph #2: " ++ show g2) g2 in
      case matchGraph gr1 gr2 of
        Right match -> return ()
        Left (largest, keys) ->
            let selectedGr1Nodes = IntMap.elems largest
                selectedGr2Nodes = IntMap.keys largest
                unselectedGr1Nodes = filter (not . flip IntSet.member (IntSet.fromList selectedGr1Nodes) . fst) (labNodes gr1)
                unselectedGr2Nodes = filter (not . flip IntSet.member (IntMap.keysSet largest) . fst) (labNodes gr2)
            in
                expectationFailure $ unlines
                    [ "Graphs weren't isomorphic."
                    , "The largest match was between"
                    , ""
                    , prettify (subgraph selectedGr1Nodes gr1)
                    , ""
                    , "and"
                    , ""
                    , prettify (subgraph selectedGr2Nodes gr2)
                    , ""
                    , "I could not match the nodes"
                    , ""
                    , show $ unselectedGr1Nodes
                    , ""
                    , "with"
                    , ""
                    , show $ unselectedGr2Nodes
                    , case keys of
                        Nothing -> ""
                        Just (k, x) -> unlines
                            [ ""
                            , "I failed when matching"
                            , show $ filter ((== x) . fst) unselectedGr1Nodes
--                            , show $ filter ((== k) . fst) unselectedGr2Nodes
                            , show unselectedGr2Nodes
                            , show k
                            ]
                    , show largest
                    ]
