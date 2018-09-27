-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- Fundamental types used in the ohua compilation process. For many
-- types this module exposes only the type, not its concrete
-- construction. This is intentional, as internal representations may
-- change. The type classes 'Make' and 'Unwrap' are provided to
-- convert to and from those types as needed.

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}

#include "compat.h"

module Ohua.Types
    ( FnId
    , Binding
    , NSRef
    , QualifiedBinding(..)
    , SomeBinding(..)
    , symbolFromString
    , ExtractBindings(..)
    , AbstractAssignment(..), Assignment
    , _Direct
    , _Destructure
    , _Recursive
    , HostExpr
    , Options
    , OhuaState
    , Environment
    , Error
    , nameGenerator
    , idCounter
    , envExpressions
    , NameGenerator
    , simpleNameList
    , takenNames
    , callEnvExpr
    , callLocalFunction
    , transformRecursiveFunctions
    , options
    , Annotated(Annotated)
    , TyExprF(..)
#if GHC_HAS_BUNDLED_PATTERN_SYNONYMS
    , TyExpr(unTyExpr, TyApp, TyRef)
#else
    , TyExpr(unTyExpr), pattern TyApp, pattern TyRef
#endif
    , TyVar(..)
    , SomeTyVar
    , DefaultTyExpr

    -- * Creating and inspecting values
    , SourceType
    , Make(make)
    , makeThrow
    , Unwrap(unwrap)
    -- ** Unsafely creating values
    , UnsafeMake(unsafeMake)
    ) where

import Protolude

import Control.Comonad
import Data.Bifoldable
import Data.Bitraversable
import Data.Default.Class
import Data.Foldable as F
import Data.Functor.Foldable as RS hiding (fold)
import Data.Generics.Uniplate.Direct
import qualified Data.HashSet as HS
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Exts
import Lens.Micro hiding ((<&>))
import Ohua.LensClasses
import Ohua.Util

IMPORT_HASHABLE

type family SourceType t

-- | Though it may seem innocuous, this class, as well as 'Unwrap',
-- are instrumental to the types defined in these modules. 'Make'
-- provides a way for someone needing to use types defined in this
-- module to safely instantiate them. 'Unwrap' provides a way of
-- inspecting the raw values stored underneath. Though caution is
-- advised with regards to the stability of that representation.
class Make t where
    -- | Safely construct a value of type @t@ from its source type
    -- potentially reporting an error.
    make :: MonadError Error m => SourceType t -> m t

-- | Same as 'make' but instead throws an exception
makeThrow :: Make t => SourceType t -> t
makeThrow = either panic identity . make

-- | Convert a value @t@ back to its source type @t@.
class Unwrap t where
    unwrap :: t -> SourceType t

-- | Unsafe version of 'make'. Constructs the type skipping the checks.
class UnsafeMake t where
    unsafeMake :: SourceType t -> t

-- | The numeric id of a function call site
newtype FnId =
    FnId Int
    deriving (Eq, Ord, Generic, Enum, Num, NFData, Hashable, Show)

type instance SourceType FnId = Int

instance UnsafeMake FnId where
    unsafeMake = FnId

instance Make FnId where
    make i
        | i < 0 =
            throwError $
            "Function id must be larger than 0, was " <> show i
        | otherwise = pure $ unsafeMake i

instance Unwrap FnId where
    unwrap (FnId i) = i

-- | A binding name
newtype Binding =
    Binding Text
    deriving (Eq, Hashable, Generic, Ord, Monoid, NFData, Show)

deriving instance Semigroup Binding

type instance SourceType Binding = Text

instance Make Binding where
    make "" = throwError "Binding cannot be empty"
    make s = pure $ unsafeMake s

instance UnsafeMake Binding where
    unsafeMake = Binding

instance Unwrap Binding where
    unwrap (Binding b) = b

instance IsString Binding where
    fromString = makeThrow . toS

-- | Hierarchical reference to a namespace
newtype NSRef =
    NSRef (V.Vector Binding)
    deriving (Eq, Generic, NFData, Ord, Show)

instance IsList NSRef where
  type Item NSRef = Binding
  fromList = makeThrow . fromList
  toList = GHC.Exts.toList . unwrap

type instance SourceType NSRef = [Binding]

instance UnsafeMake NSRef where
    unsafeMake = NSRef . V.fromList

instance Make NSRef where
    make = pure . unsafeMake

instance Unwrap NSRef where
    unwrap (NSRef l) = V.toList l

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . unwrap
    {-# INLINE hashWithSalt #-}


-- | A qualified binding. References a particular bound value inside a
-- namespace.
data QualifiedBinding = QualifiedBinding
    { qbNamespace :: NSRef
    , qbName      :: Binding
    } deriving (Eq, Generic, Ord, Show)

instance HasNamespace QualifiedBinding NSRef where
    namespace f s = (\ns' -> s {qbNamespace = ns'}) <$> f (qbNamespace s)
instance HasName QualifiedBinding Binding where
    name f s = (\n' -> s {qbName = n'}) <$> f (qbName s)

instance Hashable QualifiedBinding where
    hashWithSalt s (QualifiedBinding a b) = hashWithSalt s (a, b)

instance NFData QualifiedBinding where
    rnf (QualifiedBinding ns n) = ns `deepseq` rnf n

instance IsString QualifiedBinding where
    fromString s = case fromString s of
        Qual q -> q
        _      -> panic "unqualified binding"

-- | Utility type for parsing. Denotes a binding which may or may not
-- be qualified.
data SomeBinding
    = Unqual Binding
    | Qual QualifiedBinding
    deriving (Eq, Show)

instance Hashable SomeBinding where
    hashWithSalt s (Unqual b) = hashWithSalt s (0 :: Int, b)
    hashWithSalt s (Qual b)   = hashWithSalt s (1 :: Int, b)

instance IsString SomeBinding where
    fromString = either panic identity . symbolFromString . toS

-- | Attempt to parse a string into either a simple binding or a
-- qualified binding.  Assumes a form "name.space/value" for qualified
-- bindings.
symbolFromString :: MonadError Error m => Text -> m SomeBinding
symbolFromString s
    | T.null s = throwError "Symbols cannot be empty"
    | otherwise =
        case T.break (== '/') s of
            (symNs, slashName)
                | T.null symNs -> throwError "Unexpected '/' at start"
                | T.null slashName ->
                    Unqual <$> make symNs
                | Just ('/', symName) <- T.uncons slashName ->
                    if | (== '/') `T.find` symName /= Nothing ->
                           throwError "Too many '/' delimiters found."
                       | T.null symName -> throwError "Name cannot be empty"
                       | otherwise ->
                           do ns <-
                                  make =<<
                                  mapM
                                      make
                                      (T.split (== '.') symNs)
                              bnd <- make symName
                              pure $ Qual $ QualifiedBinding ns bnd
            _ -> throwError "Leading slash expected after `break`"

class ExtractBindings a where
    extractBindings :: a -> [Binding]
    default extractBindings :: (F.Foldable f, f b ~ a, ExtractBindings b) => a -> [Binding]
    extractBindings = foldMap extractBindings

instance ExtractBindings a => ExtractBindings [a]

instance ExtractBindings Binding where extractBindings = pure

-- | Allowed forms for the left hand side of a let binding or in a lambda input.
data AbstractAssignment binding
    = Direct !binding
    | Recursive !binding
    | Destructure ![binding]
    deriving (Eq, Functor, Traversable, F.Foldable, Show)

type Assignment = AbstractAssignment Binding

instance IsString Assignment where
    fromString = Direct . fromString

instance IsList Assignment where
    type Item Assignment = Binding
    fromList = Destructure
    toList (Destructure l) = l
    toList _ = panic "Direct return is not a list"

instance ExtractBindings Assignment

instance NFData binding => NFData (AbstractAssignment binding) where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d) = rnf d
    rnf (Recursive d) = rnf d

_Direct :: Prism' (AbstractAssignment binding) binding
_Direct =
    prism' Direct $ \case
        Direct a -> Just a
        _ -> Nothing

_Destructure :: Prism' (AbstractAssignment binding) [binding]
_Destructure =
    prism' Destructure $ \case
        Destructure a -> Just a
        _ -> Nothing

_Recursive :: Prism' (AbstractAssignment binding) binding
_Recursive =
    prism' Recursive $ \case
        Recursive r -> Just r
        _ -> Nothing

-- bindingType is the type of general bindings Binding is the type for
-- function arguments and `let`s etc which we know to always be local
-- I revised the proposal. I got rid of `Fn`. Not because of cyclic
-- dependencies but because I think that function application can be
-- more general.
-- | Numerical reference to a spliced expression from the host environment.
newtype HostExpr = HostExpr
    { unwrapHostExpr :: Int
    } deriving (Eq, Ord, Generic, Show)

-- Only exists to allow literal integers to be interpreted as host expressions
instance Num HostExpr where
    fromInteger = makeThrow . fromInteger
    (+) = intentionally_not_implemented
    (-) = intentionally_not_implemented
    (*) = intentionally_not_implemented
    abs = intentionally_not_implemented
    signum = intentionally_not_implemented

instance Hashable HostExpr where
    hashWithSalt s = hashWithSalt s . unwrapHostExpr

instance NFData HostExpr where rnf = rnf . unwrap

type instance SourceType HostExpr = Int

instance UnsafeMake HostExpr where
    unsafeMake = HostExpr

instance Make HostExpr where
    make i
        | i < 0 = throwError $ "HostExpr cannot be < 0"
        | otherwise = pure $ unsafeMake i

instance Unwrap HostExpr where
    unwrap (HostExpr i) = i

type Error = Text

data Options =
    Options !(Maybe QualifiedBinding)
            !(Maybe QualifiedBinding)
            Bool

instance Default Options where
    def =
        Options
            Nothing
            Nothing
            False -- for no we always disable this option

-- | State of the ohua compiler monad.
data OhuaState envExpr = OhuaState !NameGenerator !FnId !(V.Vector envExpr)

type instance SourceType (OhuaState envExpr) =
     (NameGenerator, FnId, V.Vector envExpr)

instance Make (OhuaState envExpr) where
    make (ng, fnid, exprs) = pure $ OhuaState ng fnid exprs

-- | The read only compiler environment
newtype Environment = Environment Options

instance Default Environment where
    def = Environment def

nameGenerator :: Lens' (OhuaState envExpr) NameGenerator
nameGenerator f (OhuaState gen counter envExprs) =
    f gen <&> \ng -> OhuaState ng counter envExprs

idCounter :: Lens' (OhuaState envExpr) FnId
idCounter f (OhuaState gen counter envExprs) =
    f counter <&> \c -> OhuaState gen c envExprs

envExpressions ::
       Lens (OhuaState envExpr) (OhuaState envExpr') (V.Vector envExpr) (V.Vector envExpr')
envExpressions f (OhuaState gen counter envExprs) = OhuaState gen counter <$> f envExprs

-- | Stateful name generator
data NameGenerator = NameGenerator !(HS.HashSet Binding) [Binding]

type instance SourceType NameGenerator = (HS.HashSet Binding, [Binding])

instance UnsafeMake NameGenerator where
    unsafeMake = uncurry NameGenerator

instance Make NameGenerator where
    make = pure . unsafeMake


takenNames :: Lens' NameGenerator (HS.HashSet Binding)
takenNames f (NameGenerator taken l) = flip NameGenerator l <$> f taken

simpleNameList :: Lens' NameGenerator [Binding]
simpleNameList f (NameGenerator taken l) = NameGenerator taken <$> f l

callEnvExpr :: Lens' Options (Maybe QualifiedBinding)
callEnvExpr f (Options c l e) = f c <&> \c' -> Options c' l e

callLocalFunction :: Lens' Options (Maybe QualifiedBinding)
callLocalFunction f (Options c l e) = f l <&> \l' -> Options c l' e

transformRecursiveFunctions :: Lens' Options Bool
transformRecursiveFunctions f (Options c l e) = f e <&> Options c l

options :: Lens' Environment Options
options f (Environment opts) = Environment <$> f opts

-- | Generic way of attaching arbitrary, alterable data to some type.
--
-- This is used primarily to add information to AST nodes, such as with the
-- 'Ohua.ALang.Lang.AnnExpr' type, wich uses 'Annotated' internally.
--
-- It is recommended to use the lenses 'annotated' and 'value' to interact with
-- this type and other annotated values, as those are a more stable and clean
-- interface.
data Annotated annotation value =
    Annotated !annotation
              !value
    deriving (Eq, Show)

instance (NFData annotation, NFData value) =>
         NFData (Annotated annotation value) where
    rnf (Annotated ann val) = ann `deepseq` rnf val

instance HasValue (Annotated annotation value) (Annotated annotation value') value value' where
    value f (Annotated ann val) = Annotated ann <$> f val

instance HasAnnotation (Annotated annotation value) (Annotated annotation' value) annotation annotation' where
    annotation f (Annotated ann val) = flip Annotated val <$> f ann

instance Bifunctor Annotated where
    bimap f g (Annotated ann val) = Annotated (f ann) (g val)

instance Bifoldable Annotated where
    bifoldr f g c (Annotated ann val) = f ann $ g val c

instance Bitraversable Annotated where
    bitraverse f g (Annotated ann val) = Annotated <$> f ann <*> g val

instance Functor (Annotated ann) where
    fmap = bimap identity

instance F.Foldable (Annotated ann) where
    foldr = bifoldr (flip const)

instance Traversable (Annotated ann) where
    traverse = bitraverse pure

instance Comonad (Annotated ann) where
    extract (Annotated _ val) = val
    duplicate x@(Annotated ann _) = Annotated ann x
    extend f a@(Annotated ann _) = Annotated ann (f a)

-- | A type expression. Similar to the AST in "Ohua.ALang.Lang" this expression
-- type leverages @recursion-schemes@ and @uniplate@ for generic traversals.
--
-- The actual expression type is 'TyExpr' and its associated patterns, 'TyRef'
-- and 'TyApp'. The 'TyExprF' type is its base functor, encountered when using
-- the @recursion-schemes@ library functions, such as 'RS.cata'.
data TyExprF binding a
    = TyRefF binding -- ^ A primitive referece to a type
    | TyAppF a a -- ^ A type application
    deriving (Show, Eq, Functor, Traversable, F.Foldable)

newtype TyExpr binding = TyExpr
    { unTyExpr :: TyExprF binding (TyExpr binding)
    } deriving (Eq, Show)

pattern TyRef :: binding -> TyExpr binding
pattern TyRef b = TyExpr (TyRefF b)

pattern TyApp :: TyExpr binding -> TyExpr binding -> TyExpr binding
pattern TyApp f v = TyExpr (TyAppF f v)

#if COMPLETE_PRAGMA_WORKS
{-# COMPLETE TyRef, TyApp #-}
#endif

type instance Base (TyExpr binding) = TyExprF binding

instance RS.RECURSION_SCHEMES_RECURSIVE_CLASS (TyExpr binding) where
    project (TyExpr e) = e
instance RS.RECURSION_SCHEMES_CORECURSIVE_CLASS (TyExpr binding) where
    embed = TyExpr

instance Uniplate (TyExpr bnd) where
    uniplate (TyRef r) = plate (TyRef r)
    uniplate (TyApp a b) = plate TyApp |* a |* b

instance Biplate (TyExpr bnd) (TyExpr bnd) where
    biplate = plateSelf

instance Uniplate bnd => Biplate (TyExpr bnd) bnd where
    biplate (TyRef r) = plate TyRef |* r
    biplate (TyApp a b) = plate TyApp |+ a |+ b

instance (NFData binding, NFData a) => NFData (TyExprF binding a) where
    rnf (TyRefF v) = rnf v
    rnf (TyAppF f v) = f `deepseq` rnf v

instance NFData binding => NFData (TyExpr binding) where
    rnf (TyExpr e) = rnf e

instance Functor TyExpr where
    fmap f (TyRef r) = TyRef $ f r
    fmap f (TyApp e1 e2) = recur e1 `TyApp` recur e2
      where recur = fmap f

instance F.Foldable TyExpr where
    foldr f =
        flip $
        cata $ \case
            TyRefF r -> f r
            TyAppF a b -> a . b

instance Traversable TyExpr where
    traverse f =
        cata $ \case
            TyRefF r -> TyRef <$> f r
            TyAppF a b -> TyApp <$> a <*> b

-- | Default primitive type references (type variables and constructors)
data TyVar tyConRef tyVarRef
    = TyCon tyConRef
    | TyVar tyVarRef
    deriving (Show, Eq)

instance (NFData tyConRef, NFData tyVarRef) =>
         NFData (TyVar tyConRef tyVarRef) where
    rnf (TyCon c) = rnf c
    rnf (TyVar v) = rnf v

instance Bifunctor TyVar where
    bimap f _ (TyCon c) = TyCon (f c)
    bimap _ g (TyVar v) = TyVar (g v)

instance Functor (TyVar a) where fmap = bimap identity

-- | Typical instantiation of a @TyVar@
type SomeTyVar = TyVar SomeBinding SomeBinding

-- | Typical instantiation of a @TyExpr@
type DefaultTyExpr = TyExpr SomeTyVar
