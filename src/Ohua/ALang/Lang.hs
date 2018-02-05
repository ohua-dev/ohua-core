-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
module Ohua.ALang.Lang where


import           Control.DeepSeq
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Hashable
import           Data.String
import           GHC.Generics
import           Lens.Micro            ((^.))
import           Ohua.LensClasses
import           Ohua.Types
import           Ohua.Util

-- bindingType is the type of general bindings
-- Binding is the type for function arguments and `let`s etc which we know to always be local

-- I revised the proposal.
-- I got rid of `Fn`. Not because of cyclic dependencies but because I think that function application can be more general.

-- | Numerical reference to a spliced expression from the host environment.
newtype HostExpr = HostExpr { unwrapHostExpr :: Int } deriving (Eq, Ord, Generic)

instance Show HostExpr where
    show = show . unwrapHostExpr

-- Only exists to allow literal integers to be interpreted as host expressions
instance Num HostExpr where
    fromInteger = HostExpr . fromInteger
    (+) = intentionally_not_implemented
    (-) = intentionally_not_implemented
    (*) = intentionally_not_implemented
    abs = intentionally_not_implemented
    signum = intentionally_not_implemented
instance Hashable HostExpr where hashWithSalt s = hashWithSalt s . unwrapHostExpr

instance NFData HostExpr

-- (Sebastian) can we treat opaque JVM objects here somehow?
-- (Justus) Several. Well have to discus what the best option is at some point,
--          because the concrete type of the host expression depends on the backend.

-- some types for `bindingType`

type RawSymbol = HostExpr

-- | Type of references which can inhabit a `Var` once the source is copletely parsed.
data Symbol a
    = Local !Binding -- ^ the basic symbols occuring in the algorithm language
    | Sf !a !(Maybe FnId) -- ^ reference to a stateful function
    | Env !HostExpr -- ^ reference to an environment object. this maybe a var or any other term of the host language.
    deriving (Show, Eq, Generic)

type ResolvedSymbol = Symbol QualifiedBinding

instance IsString ResolvedSymbol where
    fromString str = case fromString str of
        Unqual bnd -> Local bnd
        Qual q     -> Sf q Nothing

instance ExtractBindings (Symbol a) where
    extractBindings (Local l) = return l
    extractBindings _         = mempty

instance NFData a => NFData (Symbol a) where
    rnf (Local b) = rnf b
    rnf (Env e)   = rnf e
    rnf (Sf s i)  = rnf s `seq` rnf i


-- IMPORTANT: we need this to be polymorphic over `bindingType` or at least I would very much
-- recommend that, because then we can separate the generation of the algorithms language
-- and the symbol resolution.
-- If we dont separate those we'll have to reimplement the complete symbol resolution pass for
-- each frontend

-- I changed this again so that application and let both do not use lists.
-- this makes generic transformations much simpler to write, such as SSA
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
data AExprF bndType refType a
  = VarF refType -- ^ A reference to a value
  | LetF (AbstractAssignment bndType) a a -- ^ Binding of a value to one ore more identifiers
  | ApplyF a a -- ^ Function application
  | LambdaF (AbstractAssignment bndType) a -- ^ Anonymous function definition
  deriving (Functor, Foldable, Traversable, Show, Eq)

type instance Base (AExpr bndType refType) = AExprF bndType refType

newtype AExpr bndType refType = AExpr { unAExpr :: AExprF bndType refType (AExpr bndType refType) }
  deriving (Show, Eq)

instance Recursive (AExpr bndType refType) where
  project = unAExpr

instance Corecursive (AExpr bndType refType) where
  embed = AExpr

pattern Var v = AExpr (VarF v)
pattern Let a b c = AExpr (LetF a b c)
pattern Apply a b = AExpr (ApplyF a b)
pattern Lambda a b = AExpr (LambdaF a b)

instance IsString b => IsString (AExpr a b) where
    fromString = Var . fromString

instance ExtractBindings b => ExtractBindings (Expr b) where
    extractBindings = cata $ \case
      VarF b -> extractBindings b
      LetF assign a b -> extractBindings assign ++ a ++ b
      ApplyF a b -> a ++ b
      LambdaF assign a -> extractBindings assign ++ a


instance (NFData a, NFData b) => NFData (AExpr a b) where
    rnf = cata $ \case
      VarF b -> rnf b
      LetF assign a b -> assign `deepseq` a `deepseq` rnf b
      ApplyF a b -> a `deepseq` rnf b
      LambdaF assign b -> assign `deepseq` rnf b

-- | Backward compatible alias
type Expr refType = AExpr Binding refType

-- | An Alang expression with optionally type annotated bindings (let bindings and lambda arguments)
newtype AnnExpr ann bndType refType = AnnExpr { unAnnExpr :: Annotated ann (AExprF bndType refType (AnnExpr ann bndType refType)) }
  deriving Eq

pattern AnnVar ann v = AnnExpr (Annotated ann (VarF v))
pattern AnnLet ann assign val body = AnnExpr (Annotated ann (LetF assign val body))
pattern AnnApply ann f a = AnnExpr (Annotated ann (ApplyF f a))
pattern AnnLambda ann assign body = AnnExpr (Annotated ann (LambdaF assign body))

type instance Base (AnnExpr ann bndType refType) = Compose (Annotated ann) (AExprF bndType refType)

pattern AnnVarF ann v = Compose (Annotated ann (VarF v))
pattern AnnLetF ann assign val body = Compose (Annotated ann (LetF assign val body))
pattern AnnApplyF ann f a = Compose (Annotated ann (ApplyF f a))
pattern AnnLambdaF ann assign body = Compose (Annotated ann (LambdaF assign body))

instance Recursive (AnnExpr ann bndType refType) where project = Compose . unAnnExpr
instance Corecursive (AnnExpr ann bndType refType) where embed (Compose v) = AnnExpr v

type OptTyAnnExpr = AnnExpr (Maybe DefaultTyExpr) Binding

type TyAnnExpr = AnnExpr DefaultTyExpr Binding

-- | Backward compatibility alias
type Expression = Expr ResolvedSymbol


-- | Traverse an ALang expression from left to right and top down, building a new expression.
lrPrewalkExprM :: Monad m => (AExpr bndT refT -> m (AExpr bndT refT)) -> AExpr bndT refT -> m (AExpr bndT refT)
lrPrewalkExprM f e = f e >>= \case
    Let bnd val body -> Let bnd <$> lrPrewalkExprM f val <*> lrPrewalkExprM f body
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
lrPostwalkExprM :: Monad m => (AExpr bndT refT -> m (AExpr bndT refT)) -> AExpr bndT refT -> m (AExpr bndT refT)
lrPostwalkExprM f e = f =<< case e of
    Let assign val body -> Let assign <$> lrPostwalkExprM f val <*> lrPostwalkExprM f body
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
lrPostwalkExpr :: (AExpr bndT refT -> AExpr bndT refT) -> AExpr bndT refT -> (AExpr bndT refT)
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
mapBnds f = cata $ embed . \case
  LetF assign a b -> LetF (fmap f assign) a b
  LambdaF assign b -> LambdaF (fmap f assign) b
  VarF v -> VarF v
  ApplyF a b -> ApplyF a b


mapRefs :: (refT -> refT') -> AExpr bndT refT -> AExpr bndT refT'
mapRefs f = cata $ embed . \case
  VarF ref -> VarF $ f ref
  LetF assign a b -> LetF assign a b
  LambdaF assign b -> LambdaF assign b
  ApplyF a b -> ApplyF a b


removeTyAnns :: TyAnnExpr a -> Expr a
removeTyAnns = cata $ embed . (^. value) . getCompose
