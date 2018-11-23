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
  ( Lit(..)
  , FunRef(..)
  , AExpr(..)
  , Expr
  , Expression
  , mapBnds
  -- ** Convenience patterns
  , pattern Sf, pattern SfF
  -- ** The recursion schemes support types functor
  , AExprF(..)
  -- ** Additional Traversals
  , lrPostwalkExpr
  , lrPostwalkExprM, lrPrewalkExprM
--   -- ** An annotated version of the expression AST
-- #if GHC_HAS_BUNDLED_PATTERN_SYNONYMS
--   , AnnExpr(unAnnExpr, AnnVar, AnnLet, AnnApply, AnnLambda)
--   , AnnExprF(unAnnExprF, AnnVarF, AnnLetF, AnnApplyF, AnnLambdaF)
-- #else
--   , AnnExpr(unAnnExpr), pattern AnnVar, pattern AnnLet, pattern AnnApply, pattern AnnLambda
--   , AnnExprF(unAnnExprF), pattern AnnVarF, pattern AnnLetF, pattern AnnApplyF, pattern AnnLambdaF
-- #endif
--   , TyAnnExpr, OptTyAnnExpr, removeTyAnns
  ) where

import Universum

import Data.Functor.Foldable as RS
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics.Uniplate.Direct
import Language.Haskell.TH.Syntax (Lift)
import Control.Category ((>>>))

import Ohua.Types
import Ohua.LensClasses

#include "compat.h"

-------------------- Basic ALang types --------------------

data FunRef = FunRef QualifiedBinding (Maybe FnId)
    deriving (Show, Eq, Generic, Lift)

-- | Literals of kinds we expect any host language to support
data Lit
    = NumericLit !Integer -- ^ an integer literal
    | UnitLit -- ^ aka @()@
    | EnvRefLit !HostExpr -- ^ A reference to some value from the environment
    | FunRefLit FunRef -- ^ Reference to an external function
    deriving (Show, Eq, Lift, Generic)

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
data AExpr bndType
    = Var Binding
    | Lit Lit
    | Let (AbstractAssignment bndType) (AExpr bndType) (AExpr bndType)
    | Apply (AExpr bndType) (AExpr bndType)
    | Lambda (AbstractAssignment bndType) (AExpr bndType)
    deriving (Functor, Show, Eq, Lift)

type Expr = AExpr Binding
-- | Backward compatibility alias
type Expression = Expr


-------------------- Recursion schemes support --------------------

makeBaseFunctor ''AExpr

deriving instance (Lift bndType, Lift a) => Lift (AExprF bndType a)

deriving instance (Eq bndType, Eq a) => Eq (AExprF bndType a)
--deriving instance (Ord bndType, Ord refType, Ord a) => Ord (AExprF bndType refType a)

instance Container (AExprF bndType a)

-------------------- Convenience patterns --------------------

pattern Sf :: QualifiedBinding -> Maybe FnId -> AExpr bndType
pattern Sf bnd id = Lit (FunRefLit (FunRef bnd id))

pattern SfF :: QualifiedBinding -> Maybe FnId -> AExprF bndType a
pattern SfF bnd id = LitF (FunRefLit (FunRef bnd id))

-------------------- Additional type class instances --------------------

instance NFData FunRef
instance NFData Lit

instance Hashable FunRef
instance Hashable Lit

instance IsString (AExpr a) where
    fromString = fromString >>> \case
        Unqual bnd -> Var bnd
        Qual q -> Sf q Nothing

instance ExtractBindings (AExpr bnd) where
    extractBindings e = [b | Var bnds <- universe e, b <- extractBindings bnds]

instance (NFData a) => NFData (AExpr a) where
    rnf =
        cata $ \case
            VarF b -> rnf b
            LitF l -> rnf l
            LetF assign a b -> assign `deepseq` a `deepseq` rnf b
            ApplyF a b -> a `deepseq` rnf b
            LambdaF assign b -> assign `deepseq` rnf b

-------------------- Uniplate support --------------------

instance Uniplate (AExpr bndType) where
    uniplate (Let assign val body) = plate (Let assign) |* val |* body
    uniplate (Apply f v) = plate Apply |* f |* v
    uniplate (Lambda assign body) = plate (Lambda assign) |* body
    uniplate o = plate o

instance Biplate (AExpr bndType) (AExpr bndType) where
    biplate = plateSelf

instance Uniplate (AbstractAssignment bndType) => Biplate (AExpr bndType) (AbstractAssignment bndType) where
    biplate = \case
        Let assign a b -> plate Let |* assign |+ a |+ b
        Apply a b -> plate Apply |+ a |+ b
        Lambda assign b -> plate Lambda |* assign |+ b
        o -> plate o

-------------------- Additional Traversals --------------------

-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM ::
       Monad m
    => (AExpr bndT-> m (AExpr bndT))
    -> AExpr bndT
    -> m (AExpr bndT)
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
    => (AExpr bndT -> m (AExpr bndT))
    -> AExpr bndT
    -> m (AExpr bndT)
lrPostwalkExprM f e =
    f =<<
    case e of
        Let assign val body ->
            Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
        Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
        _ -> return e

-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr :: (AExpr bndT -> AExpr bndT) -> AExpr bndT -> (AExpr bndT)
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)

mapBnds :: (bndT -> bndT') -> AExpr bndT -> AExpr bndT'
mapBnds = fmap

-------------------- Annotated expressions --------------------

-- -- | An Alang expression with optionally type annotated bindings (let bindings
-- -- and lambda arguments)
-- newtype AnnExpr ann bndType = AnnExpr
--     { unAnnExpr :: Annotated ann (AExprF bndType (AnnExpr ann bndType))
--     } deriving (Eq, Lift)

-- -- | Annotated version of the 'Var' constructor pattern
-- pattern AnnVar :: ann -> Binding -> AnnExpr ann bndType

-- pattern AnnVar ann v = AnnExpr (Annotated ann (VarF v))

-- -- | Annotated version of the 'Let' constructor pattern
-- pattern AnnLet ::
--         ann ->
--           AbstractAssignment bndType ->
--             AnnExpr ann bndType ->
--               AnnExpr ann bndType -> AnnExpr ann bndType

-- pattern AnnLet ann assign val body =
--         AnnExpr (Annotated ann (LetF assign val body))

-- -- | Annotated version of the 'Apply' constructor pattern
-- pattern AnnApply ::
--         ann ->
--           AnnExpr ann bndType ->
--             AnnExpr ann bndType -> AnnExpr ann bndType

-- pattern AnnApply ann f a = AnnExpr (Annotated ann (ApplyF f a))

-- -- | Annotated version of the 'Lambda' constructor pattern
-- pattern AnnLambda ::
--         ann ->
--           AbstractAssignment bndType ->
--             AnnExpr ann bndType -> AnnExpr ann bndType

-- pattern AnnLambda ann assign body =
--         AnnExpr (Annotated ann (LambdaF assign body))
-- #if COMPLETE_PRAGMA_WORKS
-- {-# COMPLETE AnnVar, AnnLet, AnnApply, AnnLambda #-}
-- #endif

-- annExprLens ::
--        Lens (AnnExpr ann bndType) (AnnExpr ann' bndType') (Annotated ann (AExprF bndType (AnnExpr ann bndType))) (Annotated ann' (AExprF bndType' (AnnExpr ann' bndType')))
-- annExprLens app (AnnExpr e) = AnnExpr <$> app e
-- {-# INLINE annExprLens #-}

-- -- | This instance cannot change the type of @ann@, because @ann@ also occurs in
-- -- the 'value' part, which is inaccessible with this function. If you wish to
-- -- achieve this effect use a traversal like 'RS.cata' and use 'annotation' on
-- -- the functor 'AnnExprF', the instance of which /does/ have the power to change
-- -- the type.
-- instance HasAnnotation (AnnExpr ann bnd) (AnnExpr ann bnd) ann ann where
--     annotation = annExprLens . annotation

-- instance HasValue (AnnExpr ann bnd) (AnnExpr ann bnd') (AExprF bnd (AnnExpr ann bnd )) (AExprF bnd' (AnnExpr ann bnd')) where
--     value = annExprLens . value

-- -- | Base Functor for the 'AnnExpr' type
-- newtype AnnExprF ann bndType a = AnnExprF
--     { unAnnExprF :: Annotated ann (AExprF bndType a)
--     } deriving (Functor, Lift)

-- type instance Base (AnnExpr ann bndType) =
--      AnnExprF ann bndType

-- pattern AnnVarF :: ann -> Binding -> AnnExprF ann bndType a

-- pattern AnnVarF ann v = AnnExprF (Annotated ann (VarF v))

-- pattern AnnLetF ::
--         ann ->
--           AbstractAssignment bndType ->
--             a -> a -> AnnExprF ann bndType a

-- pattern AnnLetF ann assign val body =
--         AnnExprF (Annotated ann (LetF assign val body))

-- pattern AnnApplyF ::
--         ann -> a -> a -> AnnExprF ann bndType a

-- pattern AnnApplyF ann f a = AnnExprF (Annotated ann (ApplyF f a))

-- pattern AnnLambdaF ::
--         ann ->
--           AbstractAssignment bndType -> a -> AnnExprF ann bndType a

-- pattern AnnLambdaF ann assign body =
--         AnnExprF (Annotated ann (LambdaF assign body))
-- #if COMPLETE_PRAGMA_WORKS
-- {-# COMPLETE AnnVarF, AnnLetF, AnnApplyF, AnnLambdaF #-}
-- #endif
-- instance RS.RECURSION_SCHEMES_RECURSIVE_CLASS (AnnExpr ann bndType) where
--     project = AnnExprF . unAnnExpr

-- instance RS.RECURSION_SCHEMES_CORECURSIVE_CLASS (AnnExpr ann bndType) where
--     embed (AnnExprF v) = AnnExpr v

-- instance Uniplate (AnnExpr ann bndType) where
--     uniplate (AnnVar ann v) = plate AnnVar |- ann |- v
--     uniplate (AnnLet ann assign val body) =
--         plate AnnLet |- ann |- assign |* val |* body
--     uniplate (AnnApply ann f v) = plate AnnApply |- ann |* f |* v
--     uniplate (AnnLambda ann assign body) =
--         plate AnnLambda |- ann |- assign |* body

-- annExprFLens ::
--        Lens (AnnExprF ann bndType a) (AnnExprF ann' bndType' a') (Annotated ann (AExprF bndType a)) (Annotated ann' (AExprF bndType' a'))
-- annExprFLens app (AnnExprF e) = AnnExprF <$> app e
-- {-# INLINE annExprFLens #-}

-- instance HasAnnotation (AnnExprF ann bnd a) (AnnExprF ann' bnd a) ann ann' where
--     annotation = annExprFLens . annotation

-- instance HasValue (AnnExprF ann bnd a) (AnnExprF ann bnd' a') (AExprF bnd a) (AExprF bnd' a') where
--     value = annExprFLens . value

-- instance ExtractBindings (AnnExpr ann bnd) where
--     extractBindings e =
--         [b | AnnVar _ bnds <- universe e, b <- extractBindings bnds]

-- instance Biplate (AnnExpr ann bndType) (AnnExpr ann bndType) where
--     biplate = plateSelf

-- type OptTyAnnExpr = AnnExpr (Maybe DefaultTyExpr)

-- type TyAnnExpr = AnnExpr DefaultTyExpr


-- removeTyAnns :: TyAnnExpr a -> AExpr a
-- removeTyAnns = cata $ embed . (^. value)
