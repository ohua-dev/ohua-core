{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ohua.ALang.PPrint
    ( Pretty(pretty)
    , prettyAExpr
    , prettySymbol
    , prettyAbstractAssignment
    , prettyNS
    , quickRender
    ) where

import Protolude hiding (Symbol)

import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Lens.Micro
import qualified Data.HashMap.Strict as HM

import Ohua.ALang.Lang
import Ohua.ALang.NS
import Ohua.Unit
import Ohua.Types
import Ohua.LensClasses
import Ohua.Constants.HostExpr as HEConst

prettyAExpr ::
       (bndType -> Doc a)
    -> (refType -> Doc a)
    -> ((bndType -> Doc a) -> AbstractAssignment bndType -> Doc a)
    -> AExpr bndType refType
    -> Doc a
prettyAExpr prettyBnd prettyRef prettyAbstractAssign0 = fst . cata worker
  where
    prettyAssign = prettyAbstractAssign0 prettyBnd
    parenthesize (e, True) = parens e
    parenthesize (e, False) = e
    noParens = (, False)
    needParens = (, True)
    discardParens = fst
    worker =
        \case
            VarF bnd -> noParens $ prettyRef bnd
            LetF assign expr cont ->
                needParens $
                sep
                    [ "let" <+>
                      align (sep
                          [ prettyAssign assign <+> "="
                          , hang 2 $ discardParens expr
                          ])
                    , "in" <+> align (discardParens cont)
                    ]
            ApplyF fun arg ->
                needParens $ sep [discardParens fun, parenthesize arg]
            LambdaF assign body ->
                needParens $
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
    vsep
        [ "namespace" <+> pretty (ns ^. name)
        , indent 2 $
          vsep
              [ "algo imports:"
              , prettyImports (ns ^. algoImports)
              , "sf imports:"
              , prettyImports (ns ^. sfImports)
              , "symbols:"
              , indent 2 $
                vsep
                    [ sep [pretty symName <+> "=", indent 2 $ prettyDecl impl]
                    | (symName, impl) <- HM.toList $ ns ^. decls
                    ]
              ]
        ]
  where
    prettyImports =
        indent 2 .
        vsep .
        map
            (\(nsref, bnds) ->
                 fillBreak 10 (pretty nsref) <+>
                 align (fillSep $ map pretty bnds))

instance Pretty decl => Pretty (Namespace decl) where
    pretty = prettyNS pretty
