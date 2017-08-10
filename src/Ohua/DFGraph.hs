module Ohua.DFGraph where


import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Ohua.ALang.Lang
import           Ohua.DFLang.Lang
import           Ohua.Types



data Operator = Operator
    { operatorId   :: FnId
    , operatorType :: FnName
    }


data Target = Target
    { operator :: FnId
    , index    :: Int
    }


data Arc
    = Arc
        { source :: Target
        , target :: Target
        }
    | EnvArc
        { target    :: Target
        , envSource :: HostExpr
        }


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
        [ case arg of
            DFVar v -> Arc source target
              where
                source = fromMaybe (error "Undefined Binding") (HM.lookup v sources)
            DFEnvVar envExpr -> EnvArc target envExpr
        | (arg, index) <- maybe id ((:) . (,-1) . DFVar) (contextArg l) -- prepend (ctxBinding, -1) if there is a context arc
                            $ zip (callArguments l) [0..]
        , let target = Target (callSiteId l) index
        ]
