module Ohua.Frontend.Lang where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics.Uniplate.Direct

data Pat
    = VarP Binding
    | TupP [Pat]
    deriving (Show, Eq, Generic)

data Expr
    = VarE Binding
    | LitE Lit
    | LetE Pat Expr Expr
    | AppE Expr Expr
    | LambdaE Pat Expr
    | IfE Expr Expr Expr
    | MapE Expr Expr
    | BindE Expr Expr
    | StmtE Expr Expr
    | SeqE Expr Expr
    | TupE [Expr]
    deriving (Show, Eq, Generic)

makeBaseFunctor ''Pat

instance Uniplate Pat where
    uniplate = \case
        TupP ps -> plate TupP ||* ps
        other -> plate other

instance Hashable Pat
instance NFData Pat


makeBaseFunctor ''Expr

instance Uniplate Expr where
    uniplate = \case
        LetE p e1 e2 -> plate (LetE p) |* e1 |* e2
        AppE e2 e2 -> plate AppE |* e1 |* e2
        LambdaE p e -> plate (LambdaE p) |* e
        IfE e1 e2 e3 -> plate IfE |* e1 |* e2 |* e3
        MapE e1 e2 -> plate MapE |* e1 |* e2
        BindE e1 e2 -> plate BindE |* e1 |* e2
        StmtE e1 e2 -> plate StmtE |* e1 |* e2
        TupE es -> plate TupE ||* es
        SeqE e1 e2 -> plate SeqE |* e1 |* e2
        other -> plate other

instance Biplate Expr Pat where
    biplate = \case
        LetE p e1 e2 -> plate LetE |* p |+ e1 |+ e2
        AppE e2 e2 -> plate AppE |+ e1 |+ e2
        LambdaE p e -> plate LambdaE |* p |+ e
        IfE e1 e2 e3 -> plate IfE |+ e1 |+ e2 |+ e3
        MapE e1 e2 -> plate MapE |+ e1 |+ e2
        BindE e1 e2 -> plate BindE |+ e1 |+ e2
        StmtE e1 e2 -> plate StmtE |+ e1 |+ e2
        TupE es -> plate TupE ||+ es
        SeqE e1 e2 -> plate SeqE |+ e1 |+ e2
        other -> plate other

instance Hashable Expr
instance NFData Expr
