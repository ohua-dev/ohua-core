{-# LANGUAGE TemplateHaskell #-}

module Ohua.Frontend.Lang
    ( Pat(..)
    , Expr(..)
    , PatF(..)
    , ExprF(..)
    , toAlang
    , patterns
    ) where

import Ohua.Prelude

import Control.Category ((>>>))
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens.Plated (Plated, plate, gplate)
import Control.Lens (Traversal')

import Ohua.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.ALang.Lang as AL
import qualified Ohua.ALang.Refs as Refs
import Ohua.ParseTools.Refs (ifBuiltin, mkTuple, smapBuiltin)

data Pat
    = VarP Binding
    | TupP [Pat]
    | UnitP
    deriving (Show, Eq, Generic)

data Expr
    = VarE Binding
    | LitE Lit
    | LetE Pat
           Expr
           Expr
    | AppE Expr
           [Expr]
    | LamE [Pat]
           Expr -- ^ An expression creating a function
    | IfE Expr
          Expr
          Expr
    | MapE Expr
           Expr
    | BindE Expr
            Expr -- ^ Bind a state value to a function
    | StmtE Expr
            Expr -- ^ An expression with the return value ignored
    | SeqE Expr
           Expr
    | TupE [Expr] -- ^ create a tuple value that can be destructured
    deriving (Show, Eq, Generic)

patterns :: Traversal' Expr Pat
patterns f = \case
    LamE ps e -> flip LamE e <$> traverse f ps
    LetE p e1 e2 -> (\p' -> LetE p' e1 e2) <$> f p
    o -> pure o

makeBaseFunctor ''Pat

instance Plated Pat where plate = gplate

instance Hashable Pat

instance NFData Pat

makeBaseFunctor ''Expr

instance Plated Expr where plate = gplate
instance Hashable Expr
instance NFData Expr

-- | Not sure this traversal is necessary, but it makes every smap argument into
-- a lambda
--
-- I am leaving it here in case we need it later.
ensureLambdaInSmap :: (Monad m, MonadGenBnd m) => Expr -> m Expr
ensureLambdaInSmap =
    rewriteM $ \case
        MapE (LamE _ _) _ -> pure Nothing
        MapE other coll -> do
            bnd <- generateBinding
            pure $ Just $ MapE (LamE [VarP bnd] $ AppE other [VarE bnd]) coll
        _ -> pure Nothing

-- | Ensures every lambda takes at most one argument.
mkLamSingleArgument :: Expr -> Expr
mkLamSingleArgument =
    rewrite $ \case
        LamE (x1:x2:xs) b -> Just $ LamE [x1] $ LamE (x2 : xs) b
        _ -> Nothing

removeDestructuring :: (MonadGenBnd m, Monad m) => Expr -> m Expr
removeDestructuring =
    rewriteM $ \case
        LetE (TupP pats) e1 e2 -> do
            valBnd <- generateBinding
            pure $ Just $ LetE (VarP valBnd) e1 $ unstructure valBnd pats e2
        LamE [TupP pats] e -> do
            valBnd <- generateBinding
            pure $ Just $ LamE [VarP valBnd] $ unstructure valBnd pats e
        _ -> pure Nothing

giveEmptyLambdaUnitArgument :: Expr -> Expr
giveEmptyLambdaUnitArgument =
    rewrite $ \case
        LamE [] e -> Just $ LamE [UnitP] e
        _ -> Nothing

nthFun :: Expr
nthFun = LitE $ FunRefLit $ FunRef "ohua.lang/nth" Nothing

unstructure :: Binding -> [Pat] -> Expr -> Expr
unstructure valBnd =
    foldl (.) id .
    map (\(idx, pat) ->
             LetE pat $ AppE nthFun [LitE (NumericLit idx), VarE valBnd]) .
    zip [0 ..]

trans :: Expr -> AL.Expr
trans =
    cata $ \case
        VarEF b -> Var b
        LitEF l -> Lit l
        LetEF p e1 e2 -> Let (patToBnd p) e1 e2
        AppEF e1 e2
            | null e2 -> e1 `Apply` Lit UnitLit
            | otherwise -> foldl Apply e1 e2
        LamEF p e ->
            case p of
                [] -> e
                [p] -> Lambda (patToBnd p) e
                _ ->
                    error $
                    "Invariant broken: Found multi apply or destucture lambda: " <>
                    show p
        IfEF cont then_ else_ ->
            ifBuiltin `Apply` cont `Apply` Lambda "_" then_ `Apply`
            Lambda "_" else_
        MapEF function coll -> smapBuiltin `Apply` function `Apply` coll
        BindEF e1 e2 -> error "State binding not yet implemented in ALang"
        StmtEF e1 cont -> Let "_" e1 cont
        SeqEF source target -> error "Seq not yet implemented"
        TupEF parts -> foldl Apply (Sf mkTuple Nothing) parts
  where
    patToBnd =
        \case
            VarP v -> v
            UnitP -> "_"

            p -> error $ "Invariant broken, invalid pattern: " <> show p

toAlang :: (Monad m, MonadGenBnd m) => Expr -> m AL.Expr
toAlang =
    giveEmptyLambdaUnitArgument >>>
    mkLamSingleArgument >>> removeDestructuring >=> pure . trans
