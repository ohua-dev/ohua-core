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
  ( Symbol(..)
  , AExpr(..)
  , ResolvedSymbol
  , Expr
  , Expression
  , OptTyAnnExpr, TyAnnExpr
  , mapBnds, mapRefs, removeTyAnns
  -- ** The recursion schemes support types functor
  , AExprF(..)
  -- ** Additional Traversals
  , lrPostwalkExpr
  , lrPostwalkExprM, lrPrewalkExprM
  -- ** An annotated version of the expression AST
#if GHC_HAS_BUNDLED_PATTERN_SYNONYMS
  , AnnExpr(unAnnExpr, AnnVar, AnnLet, AnnApply, AnnLambda)
  , AnnExprF(unAnnExprF, AnnVarF, AnnLetF, AnnApplyF, AnnLambdaF)
#else
  , AnnExpr(unAnnExpr), pattern AnnVar, pattern AnnLet, pattern AnnApply, pattern AnnLambda
  , AnnExprF(unAnnExprF), pattern AnnVarF, pattern AnnLetF, pattern AnnApplyF, pattern AnnLambdaF
#endif
  ) where

import Universum

import Data.Bifunctor (Bifunctor(bimap, first, second))
import Data.Foldable as F
import Data.Functor.Foldable as RS
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Generics.Uniplate.Direct
import Language.Haskell.TH.Syntax (Lift)

import Ohua.Types
import Ohua.LensClasses

#include "compat.h"

-------------------- Basic ALang types --------------------

-- Discussion on `HostExpr`:
-- (Sebastian) can we treat opaque JVM objects here somehow?
-- (Justus) Several. Well have to discus what the best option is at some point,
--          because the concrete type of the host expression depends on the backend.
-- some types for `bindingType`

-- | When the source is completely parsed, a `Var` may contain any of the following:
data Symbol a
    = Local !Binding -- ^ the basic symbols occuring in the algorithm language
    | Sf !a
         !(Maybe FnId) -- ^ reference to a stateful function (with an optional identifier)
    | Env !HostExpr -- ^ reference to an environment object. this
                    -- maybe a var or any other term of the host
                    -- language.
    deriving (Show, Eq, Generic, Lift)

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
data AExpr bndType refType
    = Var refType
    | Let (AbstractAssignment bndType) (AExpr bndType refType) (AExpr bndType refType)
    | Apply (AExpr bndType refType) (AExpr bndType refType)
    | Lambda (AbstractAssignment bndType) (AExpr bndType refType)
    deriving (Functor, Show, Eq, Lift)

type ResolvedSymbol = Symbol QualifiedBinding
type Expr refType = AExpr Binding refType
-- | Backward compatibility alias
type Expression = Expr ResolvedSymbol

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''AExpr

deriving instance (Lift bndType, Lift refType, Lift a) => Lift (AExprF bndType refType a)

deriving instance (Eq bndType, Eq refType, Eq a) => Eq (AExprF bndType refType a)
--deriving instance (Ord bndType, Ord refType, Ord a) => Ord (AExprF bndType refType a)

instance Container (AExprF bndType refType a)

-------------------- Additional type class instances --------------------


instance IsString ResolvedSymbol where
    fromString str =
        case fromString str of
            Unqual bnd -> Local bnd
            Qual q -> Sf q Nothing

instance ExtractBindings (Symbol a) where
    extractBindings (Local l) = return l
    extractBindings _ = mempty

instance Uniplate (Symbol a) where
    uniplate = plate

instance NFData a => NFData (Symbol a) where
    rnf (Local b) = rnf b
    rnf (Env e) = rnf e
    rnf (Sf s i) = rnf s `seq` rnf i

instance IsString b => IsString (AExpr a b) where
    fromString = Var . fromString

instance Bifunctor AExpr where
    bimap f g = cata $ embed . \case
        VarF b -> VarF $ g b
        LetF assign a b -> LetF (fmap f assign) a b
        ApplyF a b -> ApplyF a b
        LambdaF assign a -> LambdaF (fmap f assign) a

instance ExtractBindings ref => ExtractBindings (AExpr bnd ref) where
    extractBindings e = [b | Var bnds <- universe e, b <- extractBindings bnds]

instance (NFData a, NFData b) => NFData (AExpr a b) where
    rnf =
        cata $ \case
            VarF b -> rnf b
            LetF assign a b -> assign `deepseq` a `deepseq` rnf b
            ApplyF a b -> a `deepseq` rnf b
            LambdaF assign b -> assign `deepseq` rnf b

-------------------- Uniplate support --------------------

instance Uniplate (AExpr bndType refType) where
    uniplate v@(Var _) = plate v
    uniplate (Let assign val body) = plate (Let assign) |* val |* body
    uniplate (Apply f v) = plate Apply |* f |* v
    uniplate (Lambda assign body) = plate (Lambda assign) |* body

instance Biplate (AExpr bndType refType) (AExpr bndType refType) where
    biplate = plateSelf

instance Uniplate (AbstractAssignment bndType) => Biplate (AExpr bndType refType) (AbstractAssignment bndType) where
    biplate = \case
        v@(Var _) -> plate v
        Let assign a b -> plate Let |* assign |+ a |+ b
        Apply a b -> plate Apply |+ a |+ b
        Lambda assign b -> plate Lambda |* assign |+ b

instance Uniplate refType => Biplate (AExpr bndType refType) refType where
    biplate = \case
        Var v -> plate Var |* v
        Let assign a b -> plate (Let assign) |+ a |+ b
        Apply a b -> plate Apply |+ a |+ b
        Lambda assign b -> plate (Lambda assign) |+ b

-------------------- Additional Traversals --------------------

-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM ::
       Monad m
    => (AExpr bndT refT -> m (AExpr bndT refT))
    -> AExpr bndT refT
    -> m (AExpr bndT refT)
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
    => (AExpr bndT refT -> m (AExpr bndT refT))
    -> AExpr bndT refT
    -> m (AExpr bndT refT)
lrPostwalkExprM f e =
    f =<<
    case e of
        Let assign val body ->
            Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
        Apply fn arg -> Apply <$> lrPostwalkExprM f fn <*> lrPostwalkExprM f arg
        Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
        _ -> return e

-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr ::
       (AExpr bndT refT -> AExpr bndT refT)
    -> AExpr bndT refT
    -> (AExpr bndT refT)
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)

mapBnds :: (bndT -> bndT') -> AExpr bndT refT -> AExpr bndT' refT
mapBnds = first

mapRefs :: (refT -> refT') -> AExpr bndT refT -> AExpr bndT refT'
mapRefs = second

removeTyAnns :: TyAnnExpr a -> Expr a
removeTyAnns = cata $ embed . (^. value)

-------------------- Annotated expressions --------------------

-- | An Alang expression with optionally type annotated bindings (let bindings
-- and lambda arguments)
newtype AnnExpr ann bndType refType = AnnExpr
    { unAnnExpr :: Annotated ann (AExprF bndType refType (AnnExpr ann bndType refType))
    } deriving (Eq, Lift)

-- | Annotated version of the 'Var' constructor pattern
pattern AnnVar :: ann -> refType -> AnnExpr ann bndType refType

pattern AnnVar ann v = AnnExpr (Annotated ann (VarF v))

-- | Annotated version of the 'Let' constructor pattern
pattern AnnLet ::
        ann ->
          AbstractAssignment bndType ->
            AnnExpr ann bndType refType ->
              AnnExpr ann bndType refType -> AnnExpr ann bndType refType

pattern AnnLet ann assign val body =
        AnnExpr (Annotated ann (LetF assign val body))

-- | Annotated version of the 'Apply' constructor pattern
pattern AnnApply ::
        ann ->
          AnnExpr ann bndType refType ->
            AnnExpr ann bndType refType -> AnnExpr ann bndType refType

pattern AnnApply ann f a = AnnExpr (Annotated ann (ApplyF f a))

-- | Annotated version of the 'Lambda' constructor pattern
pattern AnnLambda ::
        ann ->
          AbstractAssignment bndType ->
            AnnExpr ann bndType refType -> AnnExpr ann bndType refType

pattern AnnLambda ann assign body =
        AnnExpr (Annotated ann (LambdaF assign body))
#if COMPLETE_PRAGMA_WORKS
{-# COMPLETE AnnVar, AnnLet, AnnApply, AnnLambda #-}
#endif

annExprLens ::
       Lens (AnnExpr ann bndType refType) (AnnExpr ann' bndType' refType') (Annotated ann (AExprF bndType refType (AnnExpr ann bndType refType))) (Annotated ann' (AExprF bndType' refType' (AnnExpr ann' bndType' refType')))
annExprLens app (AnnExpr e) = AnnExpr <$> app e
{-# INLINE annExprLens #-}

-- | This instance cannot change the type of @ann@, because @ann@ also occurs in
-- the 'value' part, which is inaccessible with this function. If you wish to
-- achieve this effect use a traversal like 'RS.cata' and use 'annotation' on
-- the functor 'AnnExprF', the instance of which /does/ have the power to change
-- the type.
instance HasAnnotation (AnnExpr ann bnd ref) (AnnExpr ann bnd ref) ann ann where
    annotation = annExprLens . annotation

instance HasValue (AnnExpr ann bnd ref) (AnnExpr ann bnd' ref') (AExprF bnd ref (AnnExpr ann bnd ref)) (AExprF bnd' ref' (AnnExpr ann bnd' ref')) where
    value = annExprLens . value

-- | Base Functor for the 'AnnExpr' type
newtype AnnExprF ann bndType refType a = AnnExprF
    { unAnnExprF :: Annotated ann (AExprF bndType refType a)
    } deriving (Functor, Lift)

type instance Base (AnnExpr ann bndType refType) =
     AnnExprF ann bndType refType

pattern AnnVarF :: ann -> refType -> AnnExprF ann bndType refType a

pattern AnnVarF ann v = AnnExprF (Annotated ann (VarF v))

pattern AnnLetF ::
        ann ->
          AbstractAssignment bndType ->
            a -> a -> AnnExprF ann bndType refType a

pattern AnnLetF ann assign val body =
        AnnExprF (Annotated ann (LetF assign val body))

pattern AnnApplyF ::
        ann -> a -> a -> AnnExprF ann bndType refType a

pattern AnnApplyF ann f a = AnnExprF (Annotated ann (ApplyF f a))

pattern AnnLambdaF ::
        ann ->
          AbstractAssignment bndType -> a -> AnnExprF ann bndType refType a

pattern AnnLambdaF ann assign body =
        AnnExprF (Annotated ann (LambdaF assign body))
#if COMPLETE_PRAGMA_WORKS
{-# COMPLETE AnnVarF, AnnLetF, AnnApplyF, AnnLambdaF #-}
#endif
instance RS.RECURSION_SCHEMES_RECURSIVE_CLASS (AnnExpr ann bndType refType) where
    project = AnnExprF . unAnnExpr

instance RS.RECURSION_SCHEMES_CORECURSIVE_CLASS (AnnExpr ann bndType refType) where
    embed (AnnExprF v) = AnnExpr v

instance Uniplate (AnnExpr ann bndType refType) where
    uniplate (AnnVar ann v) = plate AnnVar |- ann |- v
    uniplate (AnnLet ann assign val body) =
        plate AnnLet |- ann |- assign |* val |* body
    uniplate (AnnApply ann f v) = plate AnnApply |- ann |* f |* v
    uniplate (AnnLambda ann assign body) =
        plate AnnLambda |- ann |- assign |* body

annExprFLens ::
       Lens (AnnExprF ann bndType refType a) (AnnExprF ann' bndType' refType' a') (Annotated ann (AExprF bndType refType a)) (Annotated ann' (AExprF bndType' refType' a'))
annExprFLens app (AnnExprF e) = AnnExprF <$> app e
{-# INLINE annExprFLens #-}

instance HasAnnotation (AnnExprF ann bnd ref a) (AnnExprF ann' bnd ref a) ann ann' where
    annotation = annExprFLens . annotation

instance HasValue (AnnExprF ann bnd ref a) (AnnExprF ann bnd' ref' a') (AExprF bnd ref a) (AExprF bnd' ref' a') where
    value = annExprFLens . value

instance ExtractBindings ref => ExtractBindings (AnnExpr ann bnd ref) where
    extractBindings e =
        [b | AnnVar _ bnds <- universe e, b <- extractBindings bnds]

instance Biplate (AnnExpr ann bndType refType) (AnnExpr ann bndType refType) where
    biplate = plateSelf

type OptTyAnnExpr = AnnExpr (Maybe DefaultTyExpr) Binding

type TyAnnExpr = AnnExpr DefaultTyExpr Binding
