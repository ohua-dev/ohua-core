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
import Data.List (partition)
import Ohua.DFLang.Lang

data Operator = Operator
    { operatorId :: !FnId
    , operatorType :: !QualifiedBinding
    } deriving (Eq, Generic, Show)

data Target = Target
    { operator :: !FnId
    , index :: !Int
    } deriving (Eq, Generic, Show)

data Arcs envExpr = Arcs
    { direct :: ![DirectArc envExpr]
    , compound :: ![Arc envExpr]
    } deriving (Eq, Generic, Show)

data Arc envExpr
    = Direct (DirectArc envExpr)
    | Compound CompoundArc
    deriving (Eq, Generic, Show)

data CompoundArc = CompoundArc
    { compoundTarget :: !Target
    , compoundSource :: ![Target]
    } deriving (Eq, Generic, Show)

data DirectArc envExpr = DirectArc
    { target :: !Target
    , source :: !(Source envExpr)
    } deriving (Eq, Generic, Show)

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

instance NFData a => NFData (DirectArc a)

instance NFData (CompoundArc)

instance NFData a => NFData (Arc a)

instance NFData a => NFData (Arcs a)

instance NFData a => NFData (Source a)

instance NFData a => NFData (AbstractOutGraph a)

toGraph :: DFExpr -> OutGraph
toGraph (DFExpr lets r) = OutGraph ops grArcs (getSource r)
  where
    ops = map toOp $ toList lets
    toOp e = Operator (callSiteId e) (deRef e)
    deRef =
        (\case
             DFFunction n -> n
             EmbedSf n -> n) .
        functionRef
    sources =
        HM.fromList $
        toList lets >>= \l ->
            [ (var, Target (callSiteId l) idx)
            | (var, idx) <- zip (output l) [0 ..]
            ]
    grArcs =
        let allArcs = toList lets
            (toDirectArcs, toCompoundArcs) =
                flip partition allArcs $ \LetExpr {callArguments = callArgs} ->
                    null $
                    flip filter callArgs $ \case
                        DFVarList _ -> False
                        _ -> True
         in Arcs
                (concatMap toDirectArc toDirectArcs)
                (concatMap toCompAndDirectArc toCompoundArcs)
    getSource v =
        fromMaybe
            (error $
             "Undefined Binding: DFVar " <> show v <> " defined vars: " <>
             show sources) $
        HM.lookup v sources
    toArc createArc l =
        [ createArc t arg
        | (arg, idx) <- zip (callArguments l) [0 ..]
        , let t = Target (callSiteId l) idx
        ]
    -- toDirectArc :: LetExpr -> [DirectArc envExpr]
    toDirectArc =
        toArc $ \t arg ->
            case arg of
                DFVar v -> DirectArc t $ LocalSource $ getSource v
                DFEnvVar envExpr -> DirectArc t $ EnvSource envExpr
                DFVarList _ -> error "Invariant Broken!"
    -- toCompAndDirectArc :: LetExpr -> [Arc envExpr]
    toCompAndDirectArc =
        toArc $ \t arg ->
            case arg of
                DFVar v -> Direct $ DirectArc t $ LocalSource $ getSource v
                DFEnvVar envExpr -> Direct $ DirectArc t $ EnvSource envExpr
                DFVarList bindings ->
                    Compound $ CompoundArc t $ map getSource bindings
-- spliceEnv :: (Int -> a) -> OutGraph -> AbstractOutGraph a
-- spliceEnv lookupExpr = fmap f where f i = lookupExpr $ unwrap i
