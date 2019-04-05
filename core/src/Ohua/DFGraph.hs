-- |
-- Module      : $Header$
-- Description : The low level dataflow graph that the compiler produces in the end.
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFGraph where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Ohua.DFLang.Lang

data Operator = Operator
    { operatorId :: !FnId
    , operatorType :: !QualifiedBinding
    , operatorNType :: !NodeType
    } deriving (Eq, Generic, Show)

data Target = Target
    { operator :: !FnId
    , index :: !Int
    } deriving (Eq, Generic, Show)

data Arcs envExpr = Arcs
    { direct :: ![DirectArc envExpr]
    , state :: ![StateArc envExpr]
    , dead :: ![DeadArc]
    } deriving (Eq, Generic, Show)

data Arc target source = Arc
    { target :: !target
    , source :: !source
    } deriving (Eq, Show, Generic)

type DirectArc envExpr = Arc Target (Source envExpr)
type StateArc envExpr = Arc FnId (Source envExpr)
-- | A dead arc is a binding created in DFLang that is unused. Hence its
-- 'target' field is the constant '()' and the 'source' is some operator at some
-- index. Mostly this is used when dataflow operators that implement the
-- argument dispatch themselves have one of their outputs be unused.
type DeadArc = Arc () Target

data Source envExpr
    = LocalSource !Target
    | EnvSource !envExpr
    deriving (Eq, Generic, Show)

-- | Graph emitted by the compiler. Abstracted over the type of
-- environment expression it contains.
data AbstractOutGraph envExpr = OutGraph
    { operators :: [Operator]
    , arcs :: Arcs envExpr
    , returnArc :: Target
    } deriving (Eq, Generic, Show)

type OutGraph = AbstractOutGraph Lit

-- instance Functor Source where
--     fmap f (EnvSource e) = EnvSource $ f e
--     fmap _ (LocalSource t) = LocalSource t
--
-- instance Functor DirectArc where
--     fmap f (DirectArc t s) = DirectArc t $ fmap f s
--
-- instance Functor AbstractOutGraph where
--     fmap f (OutGraph ops grArcs r) = OutGraph ops (fmap (fmap f) grArcs) r
instance NFData Operator

instance NFData Target

instance (NFData a, NFData b) => NFData (Arc a b)

instance (NFData a) => NFData (Arcs a)

instance NFData a => NFData (Source a)

instance NFData a => NFData (AbstractOutGraph a)

toGraph :: DFExpr -> OutGraph
toGraph (DFExpr lets r) = OutGraph ops grArcs (getSource r)
  where
    ops = map toOp $ toList lets
    states =
        mapMaybe
            (\LetExpr {..} -> fmap (Arc callSiteId . varToSource) stateArgument) $
        toList lets
    toOp LetExpr {..} =
        Operator callSiteId (nodeRef functionRef) (nodeType functionRef)
    sources =
        HM.fromList $
        toList lets >>= \l ->
            [ (var, Target (callSiteId l) idx)
            | (var, idx) <- zip (output l) [0 ..]
            ]
    grArcs =
        let directs =
                [ Arc (Target callSiteId idx) $ varToSource v
                | LetExpr {..} <- toList lets
                , (idx, v) <- zip [0 ..] callArguments
                ]
         in Arcs directs states deads
    getSource v =
        fromMaybe
            (error $
             "Undefined Binding: DFVar " <> show v <> " defined vars: " <>
             show sources) $
        HM.lookup v sources
    varToSource =
        \case
            DFVar v -> LocalSource $ getSource v
            DFEnvVar envExpr -> EnvSource envExpr
    deads =
        map (Arc () . getSource) $
        HS.toList $ allBindings `HS.difference` usedBindings
      where
        allBindings = HS.fromList $ toList lets >>= output
        usedBindings =
            HS.fromList $ r : [v | l <- toList lets, DFVar v <- callArguments l]
-- spliceEnv :: (Int -> a) -> OutGraph -> AbstractOutGraph a
-- spliceEnv lookupExpr = fmap f where f i = lookupExpr $ unwrap i
