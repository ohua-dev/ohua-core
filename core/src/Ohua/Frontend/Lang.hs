{-# LANGUAGE TemplateHaskell #-}
module Ohua.Frontend.Lang
  ( Pat(..)
  , Expr(..)
  , PatF(..)
  , ExprF(..)
  , toAlang
  ) where

import Ohua.Prelude

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics.Uniplate.Direct
import Control.Category ((>>>))

import Ohua.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.ALang.Lang as AL
import qualified Ohua.ALang.Refs as Refs
import Ohua.ParseTools.Refs (smapBuiltin, ifBuiltin)


data Pat
    = VarP Binding
    | TupP [Pat]
    | UnitP
    deriving (Show, Eq, Generic)

data Expr
    = VarE Binding
    | LitE Lit
    | LetE Pat Expr Expr
    | AppE Expr [Expr]
    | LamE [Pat] Expr -- ^ An expression creating a function
    | IfE Expr Expr Expr
    | MapE Expr Expr
    | BindE Expr Expr -- ^ Bind a state value to a function
    | StmtE Expr Expr -- ^ An expression with the return value ignored
    | SeqE Expr Expr
    | TupE [Expr] -- ^ create a tuple value that can be destructured
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
        AppE e1 e2 -> plate AppE |* e1 ||* e2
        LamE p e -> plate (LamE p) |* e
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
        AppE e1 e2 -> plate AppE |+ e1 ||+ e2
        LamE p e -> plate LamE ||* p |+ e
        IfE e1 e2 e3 -> plate IfE |+ e1 |+ e2 |+ e3
        MapE e1 e2 -> plate MapE |+ e1 |+ e2
        BindE e1 e2 -> plate BindE |+ e1 |+ e2
        StmtE e1 e2 -> plate StmtE |+ e1 |+ e2
        TupE es -> plate TupE ||+ es
        SeqE e1 e2 -> plate SeqE |+ e1 |+ e2
        other -> plate other

instance Hashable Expr
instance NFData Expr


toAlang :: MonadOhua m => Expr -> m AL.Expr
toAlang = mkSingleLamApply >>> removeDestructuring >=> trans
  where
    mkSingleLamApply =
        rewrite $ \case
            LamE (x1:x2:xs) b -> Just $ LamE [x1] $ LamE (x2 : xs) b
            other -> Nothing
    removeDestructuring =
        rewriteM $ \case
            LetE (TupP bnds) e1 e2 -> do
                valBnd <- generateBinding
                pure $ Just $ LetE (VarP valBnd) e1 $ unstructure valBnd bnds e2
            LamE [TupP bnds] e -> do
                valBnd <- generateBinding
                pure $ Just $ LamE [VarP valBnd] $ unstructure valBnd bnds e
            _ -> pure Nothing
    nthFun = LitE $ FunRefLit $ FunRef "ohua.lang/nth" Nothing
    unstructure valBnd bnds =
        foldl (.) id $
        map
            (\(idx, bnd) ->
                 LetE bnd $ AppE nthFun [LitE (NumericLit idx), VarE valBnd])
            (zip [0 ..] bnds)
    sfE name = Lit $ FunRefLit $ FunRef name Nothing
    trans =
        cata $
        sequence >=> \case
            VarEF b -> pure $ Var b
            LitEF l -> pure $ Lit l
            LetEF p e1 e2 ->
                case p of
                    VarP b -> pure $ Let b e1 e2
                    _ ->
                        throwErrorDebugS $
                        "Invariant broken: Found destructure pattern: " <> show p
            AppEF e1 e2
                | null e2 -> pure $ e1 `Apply` Lit UnitLit
                | otherwise -> pure $ foldl Apply e1 e2
            LamEF p e ->
                case p of
                    [] -> pure $ Lambda "_" e
                    [VarP b] -> pure $ Lambda b e
                    _ ->
                        throwErrorDebugS $
                        "Invariant broken: Found multi apply or destucture lambda: " <>
                        show p
            IfEF cont then_ else_ ->
                pure $
                ifBuiltin `Apply` cont `Apply` Lambda "_" then_ `Apply`
                Lambda "_" else_
            MapEF function coll -> do
                lamBnd <- generateBinding
                pure $
                    smapBuiltin `Apply`
                    Lambda lamBnd (function `Apply` Var lamBnd) `Apply`
                    coll
            BindEF e1 e2 ->
                throwError "State binding not yet implemented in ALang"
            StmtEF e1 cont -> pure $ Let "_" e1 cont
            SeqEF source target -> throwError "Seq not yet implemented"
            TupEF parts -> pure $ foldl Apply (sfE Refs.mkTuple) parts
