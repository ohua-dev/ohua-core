-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
module Ohua.Types where

import           Control.Comonad
import           Control.DeepSeq
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Default
import           Data.Functor.Classes (Show1 (liftShowsPrec))
import           Data.Hashable
import qualified Data.HashSet         as HS
import           Data.String
import qualified Data.Vector          as V
import           GHC.Exts
import           GHC.Generics
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util
import qualified Ohua.Util.Str        as Str


-- | The numeric id of a function call site
newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord, Generic, Enum, Num, NFData, Hashable)

instance Show FnId where
    show = show . unFnId

-- | A binding name
newtype Binding = Binding { unBinding :: Str.Str }
    deriving (Eq, Hashable, Generic, Ord, Monoid, NFData)

instance Show Binding where show = show . unBinding

instance IsString Binding where
    fromString = Binding . Str.fromString

type FnName = Binding

unwrapFnName :: FnName -> Binding
unwrapFnName = id
{-# DEPRECATED unwrapFnName "Use Binding or QualifiedBinding instead" #-}

-- | Hierarchical reference to a namespace
newtype NSRef = NSRef { unwrapNSRef :: V.Vector Binding }
    deriving (Eq, Generic, NFData, Ord)

instance Show NSRef where
    show = Str.toString . Str.intercalate "." . map unBinding . nsRefToList

instance IsList NSRef where
  type Item NSRef = Binding
  fromList = NSRef . fromList
  toList = toList . unwrapNSRef


-- | Creates a 'NSRef' from a hierarchical sorted list of namespaces
nsRefFromList :: [Binding] -> NSRef
nsRefFromList = NSRef . V.fromList


-- | Extract an NSRef into a list of hierarchical namespaces
nsRefToList :: NSRef -> [Binding]
nsRefToList = V.toList . unwrapNSRef

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . nsRefToList
    {-# INLINE hashWithSalt #-}


-- | A qualified binding. References a particular bound value inside a namespace.
data QualifiedBinding = QualifiedBinding
    { qbNamespace :: NSRef
    , qbName      :: Binding
    } deriving (Eq, Generic, Ord)

instance Show QualifiedBinding where
    show (QualifiedBinding ns n) = show $ show ns ++ "/" ++ Str.toString (unBinding n)

instance Hashable QualifiedBinding where
    hashWithSalt s (QualifiedBinding a b) = hashWithSalt s (a, b)

instance NFData QualifiedBinding where
    rnf (QualifiedBinding ns n) = ns `deepseq` rnf n

instance IsString QualifiedBinding where
    fromString s = case fromString s of
        Qual q -> q
        _      -> error "unqualified binding"

-- | Utility type for parsing. Denotes a binding which may or may not be qualified.
data SomeBinding
    = Unqual Binding
    | Qual QualifiedBinding
    deriving (Eq, Show)

instance Hashable SomeBinding where
    hashWithSalt s (Unqual b) = hashWithSalt s (0 :: Int, b)
    hashWithSalt s (Qual b)   = hashWithSalt s (1 :: Int, b)

instance IsString SomeBinding where
    fromString = either error id . symbolFromString

-- | Attempt to parse a string into either a simple binding or a qualified binding.
-- Assumes a form "name.space/value" for qualified bindings.
symbolFromString :: String -> Either String SomeBinding
symbolFromString s | Str.null s = Left "Symbols cannot be empty"
                   | otherwise =
    case Str.break (== '/') s of
        (symNs, slashName)
            | Str.null symNs -> Left "Unexpected '/' at start"
            | Str.null slashName -> Right $ Unqual $ Binding (Str.fromString symNs)
            | Just ('/', symName) <- Str.uncons slashName ->
                if  | '/' `Str.elem` symName -> Left "Too many '/' delimiters found."
                    | Str.null symName -> Left "Name cannot be empty"
                    | otherwise ->
                        Right $ Qual $ QualifiedBinding
                            (nsRefFromList $ map Binding $ Str.split (== '.') (fromString symNs))
                            (Binding $ fromString symName)
        _ -> error "Leading slash expected after `break`"

class ExtractBindings a where
    extractBindings :: a -> [Binding]
instance ExtractBindings a => ExtractBindings [a] where extractBindings = concatMap extractBindings


-- | Allowed forms for the left hand side of a let binding or in a lambda input.
data AbstractAssignment binding
    = Direct !binding
    | Recursive !binding
    | Destructure ![binding]
    deriving (Eq)

type Assignment = AbstractAssignment Binding

instance Functor AbstractAssignment where
    fmap f (Direct d)       = Direct $ f d
    fmap f (Recursive d)    = Direct $ f d
    fmap f (Destructure ds) = Destructure $ map f ds

instance Foldable AbstractAssignment where
    foldr f b (Direct bnd)       = f bnd b
    foldr f b (Recursive bnd)    = f bnd b
    foldr f b (Destructure bnds) = foldr f b bnds

instance Traversable AbstractAssignment where
    sequenceA (Direct a)       = Direct <$> a
    sequenceA (Destructure as) = Destructure <$> sequenceA as
    sequenceA (Recursive a)    = Recursive <$> a

instance Show binding => Show (AbstractAssignment binding) where
    show (Direct b)      = show b
    show (Destructure b) = show b
    show (Recursive b)   = "(rec) " ++ show b

instance IsString Assignment where
    fromString = Direct . fromString

instance IsList Assignment where
    type Item Assignment = Binding

    fromList = Destructure
    toList (Destructure l) = l
    toList _               = error "Direct return is not a list"

instance ExtractBindings Assignment where
    extractBindings (Direct bnd)       = [bnd]
    extractBindings (Destructure bnds) = bnds
    extractBindings (Recursive bnd)    = [bnd]

instance NFData binding => NFData (AbstractAssignment binding) where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d)       = rnf d
    rnf (Recursive d)    = rnf d

_Direct :: Prism' (AbstractAssignment binding) binding
_Direct = prism' Direct $ \case { Direct a -> Just a; _ -> Nothing }

_Destructure :: Prism' (AbstractAssignment binding) [binding]
_Destructure = prism' Destructure $ \case { Destructure a -> Just a; _ -> Nothing }

_Recursive :: Prism' (AbstractAssignment binding) binding
_Recursive = prism' Recursive $ \case { Recursive r -> Just r; _ -> Nothing }

flattenAssign :: Assignment -> [Binding]
flattenAssign = extractBindings


type Error = Str.Str

data Options = Options !(Maybe QualifiedBinding) !(Maybe QualifiedBinding) Bool

instance Default Options where
    def =
        Options
            Nothing
            Nothing
            False -- for no we always disable this option

-- | State of the ohua compiler monad.
data State envExpr = State !NameGenerator !FnId !(V.Vector envExpr)

-- | The read only compiler environment
newtype Environment = Environment Options

nameGenerator :: Lens' (State envExpr) NameGenerator
nameGenerator f (State gen counter envExprs) =
    f gen <&> \ng -> State ng counter envExprs

idCounter :: Lens' (State envExpr) FnId
idCounter f (State gen counter envExprs) = f counter <&> \c -> State gen c envExprs

envExpressions :: Lens (State envExpr) (State envExpr') (V.Vector envExpr)  (V.Vector envExpr')
envExpressions f (State gen counter envExprs) = State gen counter <$> f envExprs

-- | Stateful name generator
data NameGenerator = NameGenerator !(HS.HashSet Binding) [Binding]

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

data Annotated annotation value = Annotated !annotation !value
    deriving (Eq, Show)

instance Show annotation => Show1 (Annotated annotation) where
    liftShowsPrec showInner _ prescedence (Annotated ann val) = showParen (prescedence > app_prec) $
        showString "Annotated "
        . showsPrec (succ app_prec) ann
        . showString " "
        . showInner (succ app_prec) val
      where app_prec = 0

instance (NFData annotation, NFData value) => NFData (Annotated annotation value) where
    rnf (Annotated ann val) = ann `deepseq` rnf val

instance HasValue (Annotated annotation value) value where
    value f (Annotated ann val) = Annotated ann <$> f val

instance HasAnnotation (Annotated annotation value) annotation where
    annotation f (Annotated ann val) = flip Annotated val <$> f ann

instance Bifunctor Annotated where
    bimap f g (Annotated ann val) = Annotated (f ann) (g val)

instance Bifoldable Annotated where
    bifoldr f g c (Annotated ann val) = f ann $ g val c

instance Bitraversable Annotated where
    bitraverse f g (Annotated ann val) = Annotated <$> f ann <*> g val

instance Functor (Annotated ann) where
    fmap = bimap id

instance Foldable (Annotated ann) where
    foldr = bifoldr (flip const)

instance Traversable (Annotated ann) where
    traverse = bitraverse pure

instance Comonad (Annotated ann) where
    extract (Annotated _ val) = val
    duplicate x@(Annotated ann _) = Annotated ann x
    extend f a@(Annotated ann _) = Annotated ann (f a)

-- | A type expression
data TyExpr binding
    = TyRef binding -- ^ A primitive referece to a type
    | TyApp (TyExpr binding) (TyExpr binding) -- ^ A type application
    deriving (Show, Eq)

instance Functor TyExpr where
    fmap f (TyRef r) = TyRef $ f r
    fmap f (TyApp e1 e2) = recur e1 `TyApp` recur e2
      where recur = fmap f

instance Foldable TyExpr where
    foldr f c (TyRef r) = f r c
    foldr f c (TyApp e1 e2) = recur e1 $ recur e2 c
      where recur = flip (foldr f)

instance Traversable TyExpr where
    traverse f (TyRef r)     = TyRef <$> f r
    traverse f (TyApp e1 e2) = TyApp <$> traverse f e1 <*> traverse f e2

-- | Default primitive type references (type variables and constructors)
data TyVar tyConRef tyVarRef
    = TyCon tyConRef
    | TyVar tyVarRef
    deriving (Show, Eq)

instance Bifunctor TyVar where
    bimap f _ (TyCon c) = TyCon (f c)
    bimap _ g (TyVar v) = TyVar (g v)

instance Functor (TyVar a) where fmap = bimap id

-- | Typical instantiation of a @TyVar@
type SomeTyVar = TyVar SomeBinding SomeBinding

-- | Typical instantiation of a @TyExpr@
type DefaultTyExpr = TyExpr SomeTyVar

instance NFData binding => NFData (TyExpr binding) where
    rnf (TyRef bnd) = rnf bnd
    rnf (TyApp a b) = a `deepseq` rnf b
