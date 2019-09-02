{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ohua.ALang.PPrint
    ( Pretty(pretty)
    , prettyExpr
    , prettyLit
    , prettyNS
    , quickRender
    , ohuaDefaultLayoutOpts
    ) where

import Ohua.Prelude

import Control.Comonad (extract)
import Control.Comonad.Trans.Cofree (headF, tailF)
import Data.Functor.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Ohua.ALang.Lang
import Ohua.Frontend.NS

afterLetIndent :: Int
afterLetIndent = 0

argumentIndent :: Int
argumentIndent = 2

data Prescedence
    = VarPrec
    | AppPrec
    | BindPrec
    | LamPrec
    | LetPrec
    deriving (Eq, Ord)

type WPrec a = (Doc a, Prescedence)

parenthesize :: Prescedence -> WPrec a -> Doc a
parenthesize prec1 (e, prec0)
    | prec0 > prec1 = parens e
    | otherwise = e
noParens :: Doc a -> WPrec a
noParens = (, VarPrec)
needParens :: Prescedence -> Doc a -> WPrec a
needParens prec = (, prec)
discardParens :: WPrec a -> Doc a
discardParens = fst

prettyExpr :: Expr -> Doc a
prettyExpr = fst . histo worker
  where
    worker =
        \case
            VarF bnd -> noParens $ pretty bnd
            LitF l -> noParens $ pretty l
            LetF assign expr (extract -> cont) ->
                let (assigns, e) = collectLambdas expr
                 in needParens LetPrec $
                    sep
                        [ "let" <+>
                          align
                              (hang afterLetIndent $
                               sep
                                   [ hsep (map pretty $ assign : assigns) <+>
                                     "="
                                   , discardParens e
                                   ]) <+>
                          "in"
                        , align (discardParens cont)
                        ]
            ApplyF (extract -> fun) (extract -> arg) ->
                needParens AppPrec $
                hang argumentIndent $
                sep [parenthesize AppPrec fun, parenthesize VarPrec arg]
            LambdaF assign body ->
                let (assigns, e) = collectLambdas body
                 in needParens LamPrec $
                    "λ" <+>
                    align
                        (sep [ sep (map pretty (assign : assigns) <> ["->"])
                             , discardParens e
                             ])
            BindStateF (extract -> state) (extract -> fun) ->
                needParens BindPrec $
                hsep [discardParens fun, "with", discardParens state]
    collectLambdas =
        para $ \case
            (tailF -> LambdaF assign (_, (assigns, e))) -> (assign : assigns, e)
            (headF -> other) -> ([], other)

prettyFunRef :: FunRef -> Doc ann
prettyFunRef (FunRef sf fid) = pretty sf <> maybe emptyDoc (angles . pretty) fid

prettyLit :: Lit -> Doc ann
prettyLit =
    \case
        FunRefLit funRef -> pretty funRef
        NumericLit n -> pretty n
        UnitLit -> "()"
        EnvRefLit he -> "$" <> pretty he

instance Pretty HostExpr where
    pretty = pretty . unwrap

instance Pretty FnId where
    pretty = pretty . unwrap

instance Pretty Binding where
    pretty = pretty . (unwrap :: Binding -> Text)

instance Pretty FunRef where
    pretty = prettyFunRef

instance Pretty QualifiedBinding where
    pretty qb = pretty (qb ^. namespace) <> slash <> pretty (qb ^. name)

instance Pretty NSRef where
    pretty = hcat . punctuate dot . map pretty . unwrap

instance Pretty Expr where
    pretty = prettyExpr

instance Pretty Lit where
    pretty = prettyLit

instance Pretty SomeBinding where
    pretty (Qual q) = pretty q
    pretty (Unqual b) = pretty b


prettyNS :: (decl -> Doc a) -> Namespace decl -> Doc a
prettyNS prettyDecl ns =
    vsep $
    ("module" <+> pretty (ns ^. name)) :
    "" :
    map (prettyImport "algo") (ns ^. algoImports) <>
    map (prettyImport "sf") (ns ^. sfImports) <>
    ["", ""] <>
    intersperse "" (map prettyDecl0 (HM.toList $ ns ^. decls))
  where
    prettyImport ty (nsref, bnds) =
        "import" <+>
        ty <+> fillBreak 10 (pretty nsref) <+> align (tupled (map pretty bnds))
    prettyDecl0 (bnd, expr) =
        "let" <+>
        align (hang afterLetIndent $ sep [pretty bnd <+> "=", prettyDecl expr]) <>
        ";;"

instance Pretty decl => Pretty (Namespace decl) where
    pretty = prettyNS pretty
