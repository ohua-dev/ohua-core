{-# LANGUAGE DeriveLift #-}
module Ohua.Types.Literal where

import Universum

import Language.Haskell.TH.Syntax (Lift)

import Ohua.Types.Reference

-- | Literals of kinds we expect any host language to support
data Lit
    = NumericLit !Integer -- ^ an integer literal
    | UnitLit -- ^ aka @()@
    | EnvRefLit !HostExpr -- ^ A reference to some value from the environment
    | FunRefLit FunRef -- ^ Reference to an external function
    deriving (Show, Eq, Lift, Generic)

instance NFData Lit
instance Hashable Lit
