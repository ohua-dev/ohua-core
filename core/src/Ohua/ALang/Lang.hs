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
-- The basic building blocks are the 'AExpr' and 'Symbol'
-- type.
--
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
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.ALang.Lang
  ( Expr(..)
  , AExpr
  , Expression
  -- ** Convenience patterns
  , pattern Sf, pattern SfF
  -- ** The recursion schemes base functor
  , ExprF(..)
  -- ** Additional Traversals
  , lrPostwalkExpr
  , lrPostwalkExprM, lrPrewalkExprM
  ) where

import Universum

import Data.Functor.Foldable as RS
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Control.Lens.Plated
import Language.Haskell.TH.Syntax (Lift)
import Control.Category ((>>>))

import Ohua.Types

#include "compat.h"

-------------------- Basic ALang types --------------------


-- IMPORTANT: we need this to be polymorphic over `bindingType` or at
-- least I would very much recommend that, because then we can
-- separate the generation of the algorithms language and the symbol
-- resolution.  If we dont separate those we'll have to reimplement
-- the complete symbol resolution pass for each frontend I changed
-- this again so that application and let both do not use lists.  this
-- makes generic transformations much simpler to write, such as SSA
--
--
-- For comparison, here is the GHC intermediate language `Core`
--
-- data Expr b
    -- much like us they are polymorphic over the
    -- binding type, however in their case its the for
    -- assigments, not variables
    --
    --   = Var   Id
    --
    --   | Lit   Literal
        -- we dont need this as this is encoded in `Var`
    --
    --   | App   (Expr b) (Arg b)
        -- Expr == Arg => App ~= Apply
    --
    --   | Lam   b (Expr b)
    --   | Let   (Bind b) (Expr b)
        -- Bind == (b, Expr b) (plus recursive)
        -- I want to note that they too use single assigment
        -- let, not multiple assigment with lists
        -- same goes for lambdas and apply
    --
    --   | Case  (Expr b) b Type [Alt b]
        -- We have no case, hence we don't need this constructor
    --
    --   | Cast  (Expr b) Coercion
        -- This we might need if we get a type system
    --   | Tick  (Tickish Id) (Expr b)
        -- No clue what this is used for yet
    --
    --   | Type  Type
        -- Types ... we might need this too at some point
    --
    --   | Coercion Coercion
        -- I've heard about this, this is what makes
        -- newtypes efficient now
    --   deriving Data
    --

-- | An expression in the algorithm language.
-- Abstracted over a concrete type of local binding (target) and a type of reference (source).
data Expr
    = Var Binding
    | Lit Lit
    | Let Binding Expr Expr
    | Apply Expr Expr
    | Lambda Binding Expr
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

pattern Sf :: QualifiedBinding -> Maybe FnId -> Expr
pattern Sf bnd id = Lit (FunRefLit (FunRef bnd id))

pattern SfF :: QualifiedBinding -> Maybe FnId -> ExprF a
pattern SfF bnd id = LitF (FunRefLit (FunRef bnd id))

-------------------- Additional type class instances --------------------


instance IsString AExpr where
    fromString = fromString >>> \case
        Unqual bnd -> Var bnd
        Qual q -> Sf q Nothing

instance NFData AExpr where
    rnf =
        cata $ \case
            VarF b -> rnf b
            LitF l -> rnf l
            LetF assign a b -> assign `deepseq` a `deepseq` rnf b
            ApplyF a b -> a `deepseq` rnf b
            LambdaF assign b -> assign `deepseq` rnf b

instance Plated AExpr where plate = gplate

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
