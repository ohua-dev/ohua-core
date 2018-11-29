-- This is necessary as otherwise, for some reason the type inference
-- finds the lists to be ambiguous. I don't know why, it seems to me
-- as though it should be obvious, since the argument to `vsep` and
-- `vcat` and the likes aren't polymorphic.
{-# LANGUAGE NoOverloadedLists #-}
module Ohua.DFGraph.Show where

import Ohua.Prelude

import qualified Data.Text as T
import Text.PrettyPrint.Boxes hiding ((<>))

import Ohua.DFGraph

-- | TODO show return arc
asTable :: OutGraph -> T.Text
asTable (OutGraph ops arcs' _) =
    T.pack $
    render $
    vsep
        1
        left
        [ text "Operators"
        , hsep 4 top [idList, typeList]
        , text "Arcs"
        , hsep 4 top [sourceList, targetList]
        ]
  where
    idList =
        vcat right $ text "ids" : map (text . show . unwrap . operatorId) ops
    typeList = vcat left $ text "types" : map (text . show . operatorType) ops
    sourceList =
        vcat left $
        (text "source" :) $
        (`map` map source arcs') $ \case
            EnvSource i -> case i of
                UnitLit -> "()"
                NumericLit n -> text $ show n
                EnvRefLit n -> text $ "$" <> show (unwrap n)
                FunRefLit fr -> error "Not supported"
            LocalSource t -> targetToBox t
    targetList =
        vcat left $ (text "target" :) $ (targetToBox . target) `map` arcs'
    targetToBox (Target op idx) = text $ show (unwrap op) ++ " @ " ++ show idx
