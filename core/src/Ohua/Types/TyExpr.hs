{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Types.TyExpr where

import Universum

import Control.Lens.Plated
import qualified Data.Foldable as F
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)

import Ohua.Types.Reference

-- | Default primitive type references (type variables and constructors)
data TyVar tyConRef tyVarRef
    = TyCon tyConRef
    | TyVar tyVarRef
    deriving (Show, Eq, Functor, Lift, Generic)

-- | A type expression. Similar to the AST in "Ohua.ALang.Lang" this expression
-- type leverages @recursion-schemes@ and @uniplate@ for generic traversals.
--
-- The actual expression type is 'TyExpr' and its associated patterns, 'TyRef'
-- and 'TyApp'. The 'TyExprF' type is its base functor, encountered when using
-- the @recursion-schemes@ library functions, such as 'RS.cata'.
data TyExpr binding
    = TyRef binding -- ^ A primitive referece to a type
    | TyApp (TyExpr binding)
            (TyExpr binding) -- ^ A type application
    deriving (Show, Eq, Ord, Functor, Traversable, F.Foldable, Lift, Generic)

-- | Typical instantiation of a @TyVar@
type SomeTyVar = TyVar SomeBinding SomeBinding

-- | Typical instantiation of a @TyExpr@
type DefaultTyExpr = TyExpr SomeTyVar

instance NFData binding => NFData (TyExpr binding)
instance (NFData tyConRef, NFData tyVarRef) =>
         NFData (TyVar tyConRef tyVarRef)

makeBaseFunctor ''TyExpr

instance Plated (TyExpr binding) where plate = gplate
instance Plated (TyVar a b) where plate = gplate

instance (Hashable tyConRef, Hashable tyVarRef) =>
         Hashable (TyVar tyConRef tyVarRef)
instance Hashable binding => Hashable (TyExpr binding)

instance Bifunctor TyVar where
    bimap f _ (TyCon c) = TyCon (f c)
    bimap _ g (TyVar v) = TyVar (g v)
