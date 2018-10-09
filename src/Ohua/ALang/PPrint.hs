{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ohua.ALang.PPrint
    ( Pretty(pretty)
    , prettyAExpr
    , prettySymbol
    , prettyAbstractAssignment
    , prettyNS
    , quickRender
    ) where

import Ohua.Prelude

import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.HashMap.Strict as HM

import Ohua.ALang.Lang
import Ohua.ALang.NS
import Ohua.Constants.HostExpr as HEConst
import Ohua.Unit

prettyAExpr ::
       (bndType -> Doc a)
    -> (refType -> Doc a)
    -> ((bndType -> Doc a) -> AbstractAssignment bndType -> Doc a)
    -> AExpr bndType refType
    -> Doc a
prettyAExpr prettyBnd prettyRef prettyAbstractAssign0 = fst . cata worker
  where
    prettyAssign = prettyAbstractAssign0 prettyBnd
    parenthesize prec1 (e, prec0) | prec0 > prec1 = parens e
                                  | otherwise = e
    noParens = (, 0)
    needParens prec = (, prec)
    discardParens = fst
    worker =
        \case
            VarF bnd -> noParens $ prettyRef bnd
            LetF assign expr cont ->
                needParens 3 $
                sep
                    [ "let" <+>
                      align (sep
                          [ prettyAssign assign <+> "="
                          , hang 2 $ discardParens expr
                          ])
                    , "in" <+> align (discardParens cont)
                    ]
            ApplyF fun arg ->
                needParens 1 $ sep [parenthesize 1 fun, parenthesize 0 arg]
            LambdaF assign body ->
                needParens 2 $
                sep
                    [ "λ" <+> prettyAssign assign <+> "->"
                    , hang 2 $ discardParens body
                    ]

prettySymbol :: (a -> Doc ann) -> Symbol a -> Doc ann
prettySymbol prettySf =
    \case
        Local bnd -> pretty (unwrap bnd :: Text)
        Sf sf fid -> prettySf sf <> maybe emptyDoc (angles . pretty) fid
        Env he | he == HEConst.unit -> pretty unitBinding
               | otherwise -> "$" <> pretty he

instance Pretty HostExpr where pretty = pretty . unwrap
instance Pretty FnId where pretty = pretty . unwrap
instance Pretty Binding where pretty = pretty . (unwrap :: Binding -> Text)
instance Pretty QualifiedBinding where
    pretty qb = pretty (qb ^. namespace) <> slash <> pretty (qb ^. name)
instance Pretty NSRef where
    pretty = hcat . punctuate dot . map pretty . unwrap

instance (Pretty bndType, Pretty refType) =>
         Pretty (AExpr bndType refType) where
    pretty = prettyAExpr pretty pretty prettyAbstractAssignment

instance (Pretty bndType) => Pretty (AbstractAssignment bndType) where
    pretty = prettyAbstractAssignment pretty
instance (Pretty bndType) => Pretty (Symbol bndType) where
    pretty = prettySymbol pretty

instance Pretty SomeBinding where
    pretty (Qual q) = pretty q
    pretty (Unqual b) = pretty b


prettyAbstractAssignment :: (bndType -> Doc a) -> AbstractAssignment bndType -> Doc a
prettyAbstractAssignment prettyBnd =
    \case
        Direct b -> prettyBnd b
        Destructure bnds -> tupled $ map prettyBnd bnds
        Recursive bnd -> "rec" <+> prettyBnd bnd

quickRender :: Pretty a => a -> Text
quickRender =
    renderStrict .
    layoutSmart
        (defaultLayoutOptions {layoutPageWidth = AvailablePerLine 100 1.0}) .
    pretty

prettyNS :: (decl -> Doc a) -> Namespace decl -> Doc a
prettyNS prettyDecl ns =
    vsep $
    ("module" <+> pretty (ns ^. name)) :
    "" :
    map (prettyImport "algo") (ns ^. algoImports) <>
    map (prettyImport "sf") (ns ^. sfImports) <>
    intersperse line (map prettyDecl0 (HM.toList $ ns ^. decls))
  where
    prettyImport ty (nsref, bnds) =
        "import" <+> ty <+> fillBreak 10 (pretty nsref) <+>
        tupled (map pretty bnds)
    prettyDecl0 (bnd, expr) =
        "let" <+> pretty bnd <+> "=" <+> prettyDecl expr <> ";;"

instance Pretty decl => Pretty (Namespace decl) where
    pretty = prettyNS pretty
