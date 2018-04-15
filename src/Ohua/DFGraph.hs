-- |
-- Module      : $Header$
-- Description : The low level dataflow graph the compiler produces in the end
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFGraph where


import           Control.DeepSeq
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           GHC.Generics
import           Ohua.DFLang.Lang
import           Ohua.Types

data Operator = Operator
    { operatorId   :: !FnId
    , operatorType :: !QualifiedBinding
    } deriving (Eq, Generic, Show)

data Target = Target
    { operator :: !FnId
    , index    :: !Int
    } deriving (Eq, Generic, Show)

data Arc envExpr = Arc
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
    , arcs      :: [Arc envExpr]
    , returnArc :: Target
    } deriving (Eq, Generic, Show)

type OutGraph = AbstractOutGraph HostExpr


instance Functor Source where
    fmap f (EnvSource e)   = EnvSource $ f e
    fmap _ (LocalSource t) = LocalSource t
instance Functor Arc where
    fmap f (Arc t s) = Arc t (fmap f s)
instance Functor AbstractOutGraph where
    fmap f (OutGraph ops grArcs r) = OutGraph ops (fmap (fmap f) grArcs) r


instance NFData Operator
instance NFData Target
instance NFData a => NFData (Arc a)
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
            | (var, idx) <-
                  case returnAssignment l of
                      Direct v -> [(v, -1)]
                      Destructure vars -> zip vars [0 ..]
                      g -> error $ "Found unsupported assignment: " ++ show g
            ]
    grArcs = concatMap toArc (toList lets)
    getSource v =
        fromMaybe
            (error $
             "Undefined Binding: DFVar " ++
             show v ++ " defined vars: " ++ show sources) $
        HM.lookup v sources
    toArc l =
        [ Arc t $
        case arg of
            DFVar v -> LocalSource $ getSource v
            DFEnvVar envExpr -> EnvSource envExpr
        | (arg, idx) <-
              maybe id ((:) . (, -1) . DFVar) (contextArg l)
                  -- prepend (ctxBinding, -1) if there is a context arc
               $
              zip (callArguments l) [0 ..]
        , let t = Target (callSiteId l) idx
        ]


spliceEnv :: (Int -> a) -> OutGraph -> AbstractOutGraph a
spliceEnv lookupExpr = fmap f where f i = lookupExpr $ unwrap i

