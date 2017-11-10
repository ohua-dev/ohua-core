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
module Ohua.Types where

import           Control.DeepSeq
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.Char          as C
import           Data.Default
import           Data.Hashable
import qualified Data.HashSet       as HS
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence      as S
import           Data.String
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           GHC.Exts
import           GHC.Generics
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util


-- | The numeric id of a function call site
newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord, Generic, Enum, Num)

instance Show FnId where
    show = show . unFnId
instance Hashable FnId where hashWithSalt s = hashWithSalt s . unFnId
instance NFData FnId where rnf (FnId i) = rnf i

-- | A binding name
newtype Binding = Binding { unBinding :: T.Text }
    deriving (Eq, Hashable, Generic, Ord, Monoid)

instance Show Binding where show = show . unBinding
instance NFData Binding where rnf (Binding b) = rnf b

instance IsString Binding where
    fromString = Binding . fromString

type FnName = Binding

unwrapFnName :: FnName -> Binding
unwrapFnName = id
{-# DEPRECATED unwrapFnName "Use Binding or QualifiedBinding instead" #-}

-- | Hierarchical reference to a namespace
newtype NSRef = NSRef { unwrapNSRef :: V.Vector Binding }
    deriving (Eq, Generic)

instance Show NSRef where
    show = T.unpack . T.intercalate "." . map unBinding . nsRefToList


-- | Creates a 'NSRef' from a hierarchical sorted list of namespaces
nsRefFromList :: [Binding] -> NSRef
nsRefFromList = NSRef . V.fromList


-- | Extract an NSRef into a list of hierarchical namespaces
nsRefToList :: NSRef -> [Binding]
nsRefToList = V.toList . unwrapNSRef

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . nsRefToList
    {-# INLINE hashWithSalt #-}

instance NFData NSRef where
    rnf (NSRef a) = rnf a

-- | A qualified binding. References a particular bound value inside a namespace.
data QualifiedBinding = QualifiedBinding
    { qbNamespace :: NSRef
    , qbName      :: Binding
    } deriving (Eq, Generic)

instance Show QualifiedBinding where
    show (QualifiedBinding ns n) = show $ show ns ++ "/" ++ T.unpack (unBinding n)

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
    fromString = either error id . symbolFromString . T.pack

-- | Attempt to parse a string into either a simple binding or a qualified binding.
-- Assumes a form "name.space/value" for qualified bindings.
symbolFromString :: T.Text -> Either String SomeBinding
symbolFromString s | T.null s = Left "Symbols cannot be empty"
                   | otherwise =
    case T.break (== '/') s of
        (name, ns) | T.null name -> Left "Unexpected '/' at start"
                   | T.null ns -> Right $ Unqual $ Binding name
        (ns, rest) | Just ('/', name) <- T.uncons rest ->
            if '/' `textElem` name then
                Left "Too many '/' delimiters found."
            else
                Right $ Qual $ QualifiedBinding (nsRefFromList $ map Binding $ T.split (== '.') ns) (Binding name)
        _ -> error "Leading slash expected after `break`"

  where
    textElem c t = isJust $ (== c) `T.findIndex` t

class ExtractBindings a where
    extractBindings :: a -> [Binding]
instance ExtractBindings a => ExtractBindings [a] where extractBindings = concatMap extractBindings


-- | Allowed forms for the left hand side of a let binding or in a lambda input.
data AbstractAssignment binding
    = Direct !binding
    | Destructure ![binding]
    deriving (Eq)

type Assignment = AbstractAssignment Binding

instance Functor AbstractAssignment where
    fmap f (Direct d)       = Direct $ f d
    fmap f (Destructure ds) = Destructure $ map f ds

instance Foldable AbstractAssignment where
    foldr f b (Direct bnd)       = f bnd b
    foldr f b (Destructure bnds) = foldr f b bnds

instance Traversable AbstractAssignment where
    sequenceA (Direct a)       = Direct <$> a
    sequenceA (Destructure as) = Destructure <$> sequenceA as

instance Show binding => Show (AbstractAssignment binding) where
    show (Direct b)      = show b
    show (Destructure d) = show d

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

instance NFData binding => NFData (AbstractAssignment binding) where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d)       = rnf d

_Direct :: Prism' (AbstractAssignment binding) binding
_Direct = prism' Direct $ \case { Direct a -> Just a; _ -> Nothing }

_Destructure :: Prism' (AbstractAssignment binding) [binding]
_Destructure = prism' Destructure $ \case { Destructure a -> Just a; _ -> Nothing }


flattenAssign :: Assignment -> [Binding]
flattenAssign = extractBindings


type Error = T.Text

data Options = Options !(Maybe QualifiedBinding) !(Maybe QualifiedBinding)

instance Default Options where def = Options Nothing Nothing

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
callEnvExpr f (Options c l) = f c <&> \c' -> Options c' l

callLocalFunction :: Lens' Options (Maybe QualifiedBinding)
callLocalFunction f (Options c l) = f l <&> Options c

options :: Lens' Environment Options
options f (Environment opts) = Environment <$> f opts

data Annotated annotation value = Annotated !annotation !value
    deriving (Eq, Show)

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
    foldr f c (TyApp e1 e2) = recur e2 $ recur e2 c
      where recur = flip (foldr f)

instance Traversable TyExpr where
    traverse f (TyRef r)     = TyRef <$> f r
    traverse f (TyApp e1 e2) = TyApp <$> traverse f e1 <*> traverse f e2

-- | Default primitive type references (type variables and constructors)
data TyVar tyConRef tyVarRef
    = TyCon tyConRef
    | TyVar tyVarRef
    deriving (Show, Eq)

-- | Typical instantiation of a @TyVar@
type SomeTyVar = TyVar SomeBinding SomeBinding

-- | Typical instantiation of a @TyExpr@
type DefaultTyExpr = TyExpr SomeTyVar

instance NFData binding => NFData (TyExpr binding) where
    rnf (TyRef bnd) = rnf bnd
    rnf (TyApp a b) = a `deepseq` rnf b
