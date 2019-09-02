-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- This module defines the algorithm language. An intermediate language based on
-- the call-by-need lambda calculus.
--
-- The basic building block is the 'Expr' type.
--
-- == Traversals
--
-- ALang leverages the power of __two__ /scrap-your-boilerplate/ libraries,
-- @recursion-schemes@ and @uniplate@. Both libraries define generic ways of
-- traversing the entire expression tree easily. Depending on the function used
-- various kinds of information can be propagated up or down the tree.
--
-- Generally speaking the interface for the @uniplate@ library is easier to
-- understand for beginners, whereas the @recursion-schemes@ library is, in my
-- opinion, more type safe.
--
-- For anyone interested in learning about recursion schemes (which is also
-- applicable to @uniplate@) I recommend
-- <https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/ this multi part blog post>
-- by Patrick Thomson.
--
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.ALang.Lang
  ( Expr(..)
  , AExpr
  , Expression
  -- ** Convenience patterns
  , pattern PureFunction, pattern PureFunctionF
  , pattern StatefulFunction, pattern StatefulFunctionF
  -- ** The recursion schemes base functor
  , ExprF(..)
  -- ** Additional Traversals
  , lrPostwalkExpr
  , lrPostwalkExprM, lrPrewalkExprM
  ) where

import Universum

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens.Plated
import Language.Haskell.TH.Syntax (Lift)
import Control.Category ((>>>))

import Ohua.Types

-------------------- Basic ALang types --------------------

-- | An expression in the algorithm language.
data Expr
    = Var Binding -- ^ Reference to a value via binding: @x@ -> @Var "x"@
    | Lit Lit -- ^ A literal: @2@, @ns/func@ etc -> @Lit (NumericLit 2)@
    | Let Binding Expr Expr -- ^ Create and assign a binding: @let bnd = val in expr@ -> @Let "bnd" val expr@
    | Apply Expr Expr -- ^ Function application: @function arg@ -> @Apply function arg@
    | Lambda Binding Expr -- ^ A lambda function: @\\arg -> body@ -> @Lambda "arg" body@
    | BindState Expr Expr -- ^ Binding a state value @state#method@ -> @BindState state method@
    deriving (Show, Eq, Lift, Generic)

type AExpr = Expr
-- | Backward compatibility alias
type Expression = Expr


-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Expr

deriving instance Lift a => Lift (ExprF a)

deriving instance Eq a => Eq (ExprF a)
--deriving instance (Ord bndType, Ord refType, Ord a) => Ord (AExprF bndType refType a)

instance Container (ExprF a)

-------------------- Convenience patterns --------------------

pattern PureFunction :: QualifiedBinding -> Maybe FnId -> Expr
pattern PureFunction bnd id = Lit (FunRefLit (FunRef bnd id))

pattern PureFunctionF :: QualifiedBinding -> Maybe FnId -> ExprF a
pattern PureFunctionF bnd id = LitF (FunRefLit (FunRef bnd id))

pattern StatefulFunction :: QualifiedBinding -> Maybe FnId -> Expr -> Expr
pattern StatefulFunction bnd id expr = BindState expr (Lit (FunRefLit (FunRef bnd id)))

pattern StatefulFunctionF :: QualifiedBinding -> Maybe FnId -> Expr -> ExprF Expr
pattern StatefulFunctionF bnd id expr = BindStateF expr (Lit (FunRefLit (FunRef bnd id)))

-------------------- Additional type class instances --------------------


instance IsString AExpr where
    fromString = fromString >>> \case
        Unqual bnd -> Var bnd
        Qual q -> PureFunction q Nothing

instance NFData Expr
instance Plated Expr where plate = gplate

-------------------- Additional Traversals --------------------

-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM ::
       Monad m
    => (Expr -> m Expr)
    -> Expr
    -> m Expr
lrPrewalkExprM f e =
    f e >>= \case
        Let bnd val body ->
            Let bnd <$> lrPrewalkExprM f val <*> lrPrewalkExprM f body
        Apply fn arg -> Apply <$> lrPrewalkExprM f fn <*> lrPrewalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPrewalkExprM f body
        e' -> return e'

-- | Traverse an ALang expression from left to right and from the bottom up.
lrPostwalkExprM ::
       Monad m
    => (Expr -> m Expr)
    -> Expr
    -> m Expr
lrPostwalkExprM f e =
    f =<<
    case e of
        Let assign val body ->
            Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
        Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
        _ -> return e

-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr :: (Expr -> Expr) -> Expr -> Expr
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)
