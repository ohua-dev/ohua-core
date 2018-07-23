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
-- The best place to start would be the 'AExprF' data type. 'AExprF' is a simple
-- functor, which defines the basic alang syntax and it is the only actual type
-- present at runtime. The other expression types are either synonyms or
-- specializations which map back to the 'AExprF' type in some way.
--
-- == Different types of ALang
--
-- Alang is built around the @recursion-schemes@ library, which allows generic
-- traversals and easy manipulation of tree style recursive data types. As a
-- result all versions of alang have an underlying /base functor/ type and a top
-- level type. The "base functor" is used when pattern matching in the generic
-- functions, such as 'RS.cata', and the top level type is used when constructing
-- new values. The base functor has the same name as its corresponding top level
-- type, with an /F/ prepended. Patterns for top level types are type synonyms
-- for efficiency reasons.
--
-- The simplest type is 'AExpr' which nests only 'AExprF' into itself. The
-- convenience patterns are 'Var', 'Let', 'Apply' and 'Lambda'.
--
-- === Annotated AST
--
-- Next there is the 'AnnExpr' type which is the same as 'AExpr', but also
-- carries an annotation, via the 'Annotated' type, on every node of the AST,
-- the convenience patterns are 'AnnVar', 'AnnLet', 'AnnApply' and 'AnnLambda'.
-- 'AnnExprF' is its base functor which itself maps back to 'AExprF'
-- constructors.
--
-- The two lenses from 'Annotated', 'annotation' and 'value' also work on
-- 'AnnExpr' and 'AnnExprF'.
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
-- == API Stability
--
-- The unwrapping functions, such as 'unAExpr', 'unAnnExpr' and 'unAnnExprF'
-- should be considered unstable.
--
-- The patterns, such as 'Var', 'AnnLambda' etc should be considered the stable
-- interface, and be preferred over unwrapping of the newtypes.
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ohua.ALang.Lang
  (
  -- * Various kinds of expression types
  -- ** The fundamental expression type and its base functor
    AExprF(..)
#if GHC_HAS_BUNDLED_PATTERN_SYNONYMS
  , AExpr(unAExpr, Var, Let, Apply, Lambda)
#else
  , AExpr(unAExpr), pattern Var, pattern Let, pattern Apply, pattern Lambda
#endif
  -- ** An annotated version of the expression AST
#if GHC_HAS_BUNDLED_PATTERN_SYNONYMS
  , AnnExpr(unAnnExpr, AnnVar, AnnLet, AnnApply, AnnLambda)
  , AnnExprF(unAnnExprF, AnnVarF, AnnLetF, AnnApplyF, AnnLambdaF)
#else
  , AnnExpr(unAnnExpr), pattern AnnVar, pattern AnnLet, pattern AnnApply, pattern AnnLambda
  , AnnExprF(unAnnExprF), pattern AnnVarF, pattern AnnLetF, pattern AnnApplyF, pattern AnnLambdaF
#endif
  -- * Other basic types
  , Symbol(..)
  , ResolvedSymbol
  -- ** Common type aliases
  , OptTyAnnExpr, TyAnnExpr, Expression
  , Expr
  -- * Traversals not provided by recursions schemes or generic-deriving
  , lrPrewalkExprM, lrPostwalkExprM, lrPostwalkExpr
  , mapBnds, mapRefs, removeTyAnns
  ) where

import Control.DeepSeq
import Data.Foldable as F
import Data.Functor.Foldable as RS
import Data.Functor.Identity
import Data.Generics.Uniplate.Direct
import Data.String
import GHC.Generics
import Lens.Micro ((^.),Lens)
import Ohua.LensClasses
import Ohua.Types

#include "compat.h"

-- Discussion on `HostExpr`:
-- (Sebastian) can we treat opaque JVM objects here somehow?
-- (Justus) Several. Well have to discus what the best option is at some point,
--          because the concrete type of the host expression depends on the backend.
-- some types for `bindingType`

-- | Type of references which can inhabit a `Var` once the source is copletely parsed.
data Symbol a
    = Local !Binding -- ^ the basic symbols occuring in the algorithm language
    | Sf !a
         !(Maybe FnId) -- ^ reference to a stateful function
    | Env !HostExpr -- ^ reference to an environment object. this
                    -- maybe a var or any other term of the host
                    -- language.
    deriving (Show, Eq, Generic)

type ResolvedSymbol = Symbol QualifiedBinding

instance IsString ResolvedSymbol where
    fromString str =
        case fromString str of
            Unqual bnd -> Local bnd
            Qual q -> Sf q Nothing

instance ExtractBindings (Symbol a) where
    extractBindings (Local l) = return l
    extractBindings _ = mempty

instance NFData a => NFData (Symbol a) where
    rnf (Local b) = rnf b
    rnf (Env e) = rnf e
    rnf (Sf s i) = rnf s `seq` rnf i
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
-- | An expression in the algorithm language.
-- Abstracted over a concrete type of local binding (target) and a type of reference (source).
--
-- This is the "base functor" for an 'Expression'. Which means this construct is
-- generic in its subterms to be in accordance with the @recursion-schemes@
-- library. This type is usually not used directly, instead the 'Var', 'Let'
-- etc. patterns are used. The actual constructors of this type are encountered
-- when using functions from the @recursion-schemes@ library, such as 'RS.cata' and
-- 'RS.para'.
data AExprF bndType refType a
    = VarF refType -- ^ A reference to a value
    | LetF (AbstractAssignment bndType)
           a
           a -- ^ Binding of a value to one ore more identifiers
    | ApplyF a
             a -- ^ Function application
    | LambdaF (AbstractAssignment bndType)
              a -- ^ Anonymous function definition
    deriving (Functor, F.Foldable, Traversable, Show, Eq)

type instance Base (AExpr bndType refType) = AExprF bndType refType

-- | A type alias which recurses the 'AExprF' type onto itself.
newtype AExpr bndType refType = AExpr
    { unAExpr :: AExprF bndType refType (AExpr bndType refType)
    } deriving (Show, Eq)

instance RECURSION_SCHEMES_RECURSIVE_CLASS (AExpr bndType refType) where
    project = unAExpr

instance RECURSION_SCHEMES_CORECURSIVE_CLASS (AExpr bndType refType) where
    embed = AExpr

-- | A convenience alias for a 'VarF' in the recursive 'AExpr' type
pattern Var :: refType -> AExpr bndType refType

pattern Var v = AExpr (VarF v)

-- | A convenience alias for a 'LetF' in the recursive 'AExpr' type
pattern Let ::
        AbstractAssignment bndType ->
          AExpr bndType refType ->
            AExpr bndType refType -> AExpr bndType refType

pattern Let a b c = AExpr (LetF a b c)

-- | A convenience alias for a 'ApplyF' in the recursive 'AExpr' type
pattern Apply ::
        AExpr bndType refType ->
          AExpr bndType refType -> AExpr bndType refType

pattern Apply a b = AExpr (ApplyF a b)

-- | A convenience alias for a 'LambdaF' in the recursive 'AExpr' type
pattern Lambda ::
        AbstractAssignment bndType ->
          AExpr bndType refType -> AExpr bndType refType

pattern Lambda a b = AExpr (LambdaF a b)
#if COMPLETE_PRAGMA_WORKS
{-# COMPLETE Var, Let, Apply, Lambda #-}
#endif
instance IsString b => IsString (AExpr a b) where
    fromString = Var . fromString

instance ExtractBindings ref => ExtractBindings (AExpr bnd ref) where
    extractBindings e = [b | Var bnds <- universe e, b <- extractBindings bnds]

instance (NFData a, NFData b) => NFData (AExpr a b) where
    rnf =
        cata $ \case
            VarF b -> rnf b
            LetF assign a b -> assign `deepseq` a `deepseq` rnf b
            ApplyF a b -> a `deepseq` rnf b
            LambdaF assign b -> assign `deepseq` rnf b

instance Uniplate (AExpr bndType refType) where
    uniplate (Var v) = plate Var |- v
    uniplate (Let assign val body) = plate Let |- assign |* val |* body
    uniplate (Apply f v) = plate Apply |* f |* v
    uniplate (Lambda assign body) = plate Lambda |- assign |* body

instance Biplate (AExpr bndType refType) (AExpr bndType refType) where
    biplate = plateSelf

-- instance Uniplate refType => Biplate (AExpr bndType refType) refTyp where
--   biplate (Var v)               = plate Var |* v
--   biplate (Let assign val body) = plate Let |- assign |- val |- body
--   biplate (Apply f v)           = plate Apply |- f |- v
--   biplate (Lambda assign body)  = plate Lambda |- assign |- body
-- instance Biplate (AExpr bndType refType) (AbstractAssignment bndType) where
--   biplate (Var v)               = plate Var |- v
--   biplate (Let assign val body) = plate Let |* assign |- val |- body
--   biplate (Apply f v)           = plate Apply |- f |- v
--   biplate (Lambda assign body)  = plate Lambda |* assign |- body
-- | Backward compatible alias
type Expr refType = AExpr Binding refType

-- | An Alang expression with optionally type annotated bindings (let bindings
-- and lambda arguments)
newtype AnnExpr ann bndType refType = AnnExpr
    { unAnnExpr :: Annotated ann (AExprF bndType refType (AnnExpr ann bndType refType))
    } deriving (Eq)

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
    } deriving (Functor)

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
instance RECURSION_SCHEMES_RECURSIVE_CLASS (AnnExpr ann bndType refType) where
    project = AnnExprF . unAnnExpr

instance RECURSION_SCHEMES_CORECURSIVE_CLASS (AnnExpr ann bndType refType) where
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

-- | Backward compatibility alias
type Expression = Expr ResolvedSymbol

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

-- -- | Traverse an ALang expression from right to left and top down.
-- rlPrewalkExprM :: Monad m => (AExpr bndT refT -> m (AExpr bndT refT)) -> AExpr bndT refT -> m (AExpr bndT refT)
-- rlPrewalkExprM f e = f e >>= \case
--   Let bnd val body -> flip (Let bnd) <$> rlPrewalkExprM f body <*> rlPrewalkExprM f val
--   Apply fn arg -> flip Apply <$> rlPrewalkExprM f arg <*> rlPrewalkExprM f fn
--   Lambda assign body -> Lambda assign <$> rlPrewalkExprM f body
--   e' -> return e'
-- -- | Same as 'lrPrewalkExprM' but does not carry a monadic value.
-- lrPrewalkExpr :: (AExpr bndT refT -> AExpr bndT refT) -> AExpr bndT refT -> (AExpr bndT refT)
-- lrPrewalkExpr f = runIdentity . lrPrewalkExprM (return . f)
-- -- | Same as 'rlPrewalkExprM' but does not carry a monadic value.
-- rlPrewalkExpr :: (AExpr bndT refT -> AExpr bndT refT) -> AExpr bndT refT -> (AExpr bndT refT)
-- rlPrewalkExpr f = runIdentity . rlPrewalkExprM (return . f)
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

-- -- | Traverse an ALang expression from right to left and from the bottom up.
-- rlPostwalkExprM :: Monad m => (AExpr bndT refT -> m (AExpr bndT refT)) -> AExpr bndT refT -> m (AExpr bndT refT)
-- rlPostwalkExprM f e = f =<< case e of
--     Let assign val body -> flip (Let assign) <$> lrPostwalkExprM f body <*> lrPostwalkExprM f val
--     Apply fn arg -> flip Apply <$> lrPostwalkExprM f arg <*>  lrPostwalkExprM f fn
--     Lambda assign body -> Lambda assign <$> lrPostwalkExprM f body
--     _ -> return e
-- | Same as 'lrPostwalkExprM' but does not carry a monad.
lrPostwalkExpr ::
       (AExpr bndT refT -> AExpr bndT refT)
    -> AExpr bndT refT
    -> (AExpr bndT refT)
lrPostwalkExpr f = runIdentity . lrPostwalkExprM (return . f)

-- -- | Same as 'lrPostwalkExprM' bot does not carry a monad.
-- rlPostwalkExpr :: (AExpr bndT refT -> AExpr bndT refT) -> AExpr bndT refT -> (AExpr bndT refT)
-- rlPostwalkExpr f = runIdentity . rlPostwalkExprM (return . f)
-- -- | Generic fold over an ALang expression.
-- -- Folds from top down and from left to right.
-- foldlExprM :: Monad m => (AExpr bndT refT -> b -> m b) -> b -> AExpr bndT refT -> m b
-- foldlExprM f b e = do
--     b' <- f e b
--     case e of
--         Apply e1 e2 -> do
--             b1 <- foldlExprM f b' e1
--             foldlExprM f b1 e2
--         Let _ e1 e2 -> do
--             b1 <- foldlExprM f b' e1
--             foldlExprM f b1 e2
--         Lambda _ e1 -> foldlExprM f b' e1
--         _ -> return b'
-- -- | Generic fold over an ALang expression.
-- -- Folds from bottom up and from right to left.
-- foldrExprM :: Monad m => (a -> AExpr bndT refT -> m a) -> AExpr bndT refT -> a -> m a
-- foldrExprM f e a = do
--     a' <- case e of
--         Apply e1 e2 -> do
--             a2 <- foldrExprM f e2 a
--             foldrExprM f e1 a2
--         Let _ e1 e2 -> do
--             a2 <- foldrExprM f e2 a
--             foldrExprM f e1 a2
--         Lambda _ e1 -> foldrExprM f e1 a
--         _ -> return a
--     f a' e
-- -- | Same as 'foldlExprM' but does not carry a monad.
-- foldlExpr :: (AExpr bndT refT -> b -> b) -> b -> AExpr bndT refT -> b
-- foldlExpr f b e = runIdentity $ foldlExprM (\x y -> return $ f x y) b e
-- -- | Same as 'foldrExprM' but does not carry a monad.
-- foldrExpr :: (a -> AExpr bndT refT -> a) -> AExpr bndT refT -> a -> a
-- foldrExpr f e b = runIdentity $ foldrExprM (\x y -> return $ f x y) e b
-- lrMapBndsRefsA :: Applicative f => (bndT -> f bndT') -> (refT -> f refT') -> AExpr bndT refT -> f (AExpr bndT' refT')
-- lrMapBndsRefsA = bitraverse
-- lrMapBndsA :: Applicative f => (bndT -> f bndT') -> AExpr bndT refT -> f (AExpr bndT' refT)
-- lrMapBndsA = flip bitraverse pure
-- lrMapRefsA :: Applicative f => (refT -> f refT') -> AExpr bndT refT -> f (AExpr bndT refT')
-- lrMapRefsA = bitraverse pure
-- lrMapBndsRefs :: (bndT -> bndT') -> (refT -> refT') -> AExpr bndT refT -> AExpr bndT' refT'
-- lrMapBndsRefs = bimap
mapBnds :: (bndT -> bndT') -> AExpr bndT refT -> AExpr bndT' refT
mapBnds f =
    cata $
    embed . \case
        LetF assign a b -> LetF (fmap f assign) a b
        LambdaF assign b -> LambdaF (fmap f assign) b
        VarF v -> VarF v
        ApplyF a b -> ApplyF a b

mapRefs :: (refT -> refT') -> AExpr bndT refT -> AExpr bndT refT'
mapRefs f =
    cata $
    embed . \case
        VarF ref -> VarF $ f ref
        LetF assign a b -> LetF assign a b
        LambdaF assign b -> LambdaF assign b
        ApplyF a b -> ApplyF a b

removeTyAnns :: TyAnnExpr a -> Expr a
removeTyAnns = cata $ embed . (^. value)
