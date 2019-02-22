{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.DFLang.PPrint where

import Ohua.Prelude

import Data.Text.Prettyprint.Doc as PP

import Ohua.ALang.PPrint ()
import Ohua.DFLang.Lang

prettyDFExpr :: DFExpr -> Doc a
prettyDFExpr DFExpr {..} =
    vsep $ map prettyLetExpr (toList letExprs) <> [pretty returnVar]

instance Pretty DFExpr where
    pretty = prettyDFExpr

prettyLetExpr :: LetExpr -> Doc a
prettyLetExpr LetExpr {..} =
    hsep $
    [ "let"
    , align $ tupled $ map pretty output
    , "="
    , pretty functionRef <> angles (pretty callSiteId)
    ] <>
    maybe [] (pure . brackets . pretty) stateArgument <>
    [align $ tupled $ map pretty callArguments, "in"]

instance Pretty LetExpr where
    pretty = prettyLetExpr

prettyDFVar :: DFVar -> Doc a
prettyDFVar = \case
    DFEnvVar he -> pretty he
    DFVar b -> pretty b

instance Pretty DFVar where
    pretty = prettyDFVar

prettyDFFnRef :: DFFnRef -> Doc a
prettyDFFnRef (DFFunction f) = "dataflow" <+> pretty f
prettyDFFnRef (EmbedSf f) = pretty f

instance Pretty DFFnRef where
    pretty = prettyDFFnRef
