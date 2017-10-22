module Ohua.DFGraph.Show where

import           Ohua.ALang.Lang        (unwrapHostExpr)
import           Ohua.DFGraph
import           Ohua.Types             (unFnId)
import           Text.PrettyPrint.Boxes


printAsTable :: OutGraph -> IO ()
printAsTable (OutGraph ops arcs) = printBox $ vsep 1 left
    [ text "Operators"
    , hsep 4 top [idList, typeList ]
    , text "Arcs"
    , hsep 4 top [sourceList, targetList]
    ]
  where
    idList = vcat right $ text "ids" : map (text . show . unFnId . operatorId) ops
    typeList = vcat left $ text "types" : map (text . show . operatorType) ops
    sourceList = vcat left $ (text "source" :) $ (`map` map source arcs ) $ \case
        EnvSource i -> text $ show (unwrapHostExpr i)
        LocalSource t -> targetToBox t
    targetList = vcat left $ (text "target" :) $ (targetToBox . target) `map` arcs

    targetToBox (Target op idx) = text $ show (unFnId op) ++ " @ " ++ show idx
