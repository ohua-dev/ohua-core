-- |
-- Module      : $Header$
-- Description : The low level dataflow graph the compiler produces in the end
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFGraph where


import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang
import           Ohua.Types



data Operator = Operator
    { operatorId   :: !FnId
    , operatorType :: !FnName
    }


data Target = Target
    { operator :: !FnId
    , index    :: !Int
    }


data Arc = Arc
    { target :: !Target
    , source :: !Source
    }
    
data Source
    = LocalSource !Target
    | EnvSource !HostExpr


data OutGraph = OutGraph
    { operators :: [Operator]
    , arcs      :: [Arc]
    }



toGraph :: DFExpr -> OutGraph
toGraph (DFExpr lets _) = OutGraph ops arcs
  where
    ops = map toOp $ toList lets
    toOp e = Operator (callSiteId e) (deRef e)

    deRef = (\case DFFunction n -> n; EmbedSf n -> n) . functionRef

    sources =
        HM.fromList
        $ toList lets
            >>= \l ->
                    [ (var, Target (callSiteId l) index)
                    | (var, index) <- case returnAssignment l of
                                        Direct v         -> [(v, -1)]
                                        Destructure vars -> zip vars [0..]
                    ]

    arcs = concatMap toArc (toList lets)

    toArc l =
        [ Arc target $ 
            case arg of
                DFVar v ->  LocalSource $ fromMaybe (error "Undefined Binding") (HM.lookup v sources)
                DFEnvVar envExpr -> EnvSource envExpr
        | (arg, index) <- maybe id ((:) . (,-1) . DFVar) (contextArg l) -- prepend (ctxBinding, -1) if there is a context arc
                            $ zip (callArguments l) [0..]
        , let target = Target (callSiteId l) index
        ]
