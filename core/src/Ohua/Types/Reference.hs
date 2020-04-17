{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveLift, MultiWayIf #-}
module Ohua.Types.Reference where

import Universum

import Control.Lens.Plated
import Control.Lens.TH
import Control.Monad.Error.Class (MonadError)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Exts (IsList(..))
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Ohua.LensClasses

import Ohua.Util

import Ohua.Types.Base
import Ohua.Types.Make

-- | A binding name
newtype Binding =
    Binding Text
    deriving (Eq, Hashable, Generic, Ord, Monoid, Semigroup, NFData, Show, Lift)

-- | Hierarchical reference to a namespace
newtype NSRef =
    NSRef (V.Vector Binding)
    deriving (Eq, Generic, NFData, Ord, Show, Lift)

-- | A qualified binding. References a particular bound value inside a
-- namespace.
declareFields [d|
    data QualifiedBinding = QualifiedBinding
        { qualifiedBindingNamespace :: NSRef
        , qualifiedBindingName      :: Binding
        } deriving (Eq, Generic, Ord, Show, Lift)
  |]

-- | Utility type for parsing. Denotes a binding which may or may not
-- be qualified.
data SomeBinding
    = Unqual Binding
    | Qual QualifiedBinding
    deriving (Eq, Show, Lift, Generic)


-- | The numeric id of a function call site
newtype FnId =
    FnId Int
    deriving (Eq, Ord, Generic, Enum, Num, NFData, Hashable, Show, Lift)

-- | Numerical reference to a spliced expression from the host environment.
newtype HostExpr = HostExpr
    { unwrapHostExpr :: Int
    } deriving (Eq, Ord, Generic, Show, Lift, Hashable, NFData)

data FunRef = FunRef QualifiedBinding (Maybe FnId)
    deriving (Show, Eq, Generic, Lift)


--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

instance NFData QualifiedBinding
instance NFData SomeBinding
instance NFData FunRef

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . unwrap
    {-# INLINE hashWithSalt #-}
instance Hashable QualifiedBinding
instance Hashable SomeBinding
instance Hashable FunRef

type instance SourceType FnId = Int
type instance SourceType Binding = Text
type instance SourceType NSRef = [Binding]
type instance SourceType HostExpr = Int


instance UnsafeMake FnId where unsafeMake = FnId
instance UnsafeMake Binding where unsafeMake = Binding
instance UnsafeMake HostExpr where unsafeMake = HostExpr
instance UnsafeMake NSRef where
    unsafeMake = NSRef . V.fromList



instance Make FnId where
    make i
        | i < 0 =
            throwErrorDebugS $
            "Function id must be larger than 0, was " <> show i
        | otherwise = pure $ unsafeMake i

instance Make Binding where
    make "" = throwErrorDebugS "Binding cannot be empty"
    make s = pure $ unsafeMake s

instance Make NSRef where
    make = pure . unsafeMake

instance Make HostExpr where
    make i
        | i < 0 = throwErrorS $ "HostExpr cannot be < 0"
        | otherwise = pure $ unsafeMake i


instance Unwrap Binding where
    unwrap (Binding b) = b

instance Unwrap NSRef where
    unwrap (NSRef l) = V.toList l

instance Unwrap FnId where
    unwrap (FnId i) = i

instance Unwrap HostExpr where
    unwrap (HostExpr i) = i

instance Plated FnId where plate = gplate
instance Plated QualifiedBinding where plate = gplate
instance Plated HostExpr where plate = gplate
instance Plated Binding where plate = gplate
instance Plated NSRef where plate = gplate


instance IsString Binding where
    fromString = makeThrow . toText

instance IsString QualifiedBinding where
    fromString s = case fromString s of
        Qual q   -> q
        Unqual b -> error $ fromString $ "Encountered unqualified binding: " ++ (show b)

instance IsString SomeBinding where
    fromString = either error id . symbolFromString . toText

instance IsList NSRef where
    type Item NSRef = Binding
    fromList = makeThrow . fromList
    toList = GHC.Exts.toList . unwrap

-- Only exists to allow literal integers to be interpreted as host expressions
instance Num HostExpr where
    fromInteger = makeThrow . fromInteger
    (+) = intentionally_not_implemented
    (-) = intentionally_not_implemented
    (*) = intentionally_not_implemented
    abs = intentionally_not_implemented
    signum = intentionally_not_implemented

-- | Attempt to parse a string into either a simple binding or a
-- qualified binding.  Assumes a form "name.space/value" for qualified
-- bindings.
symbolFromString :: MonadError Error m => Text -> m SomeBinding
symbolFromString s
    | T.null s = throwErrorDebugS "Symbols cannot be empty"
    | otherwise =
        case T.break (== '/') s of
            (symNs, slashName)
                | T.null symNs ->
                    throwErrorDebugS $
                    "An unqualified name cannot start with a '/': " <> show s
                | T.null slashName -> Unqual <$> make symNs
                | Just ('/', symName) <- T.uncons slashName ->
                    if | (== '/') `T.find` symName /= Nothing ->
                           throwErrorDebugS $
                           "Too many '/' delimiters found in the binding " <>
                           show s
                       | T.null symName ->
                           throwErrorDebugS $
                           "Name cannot be empty in the binding " <> show s
                       | otherwise ->
                           do ns <- make =<< mapM make (T.split (== '.') symNs)
                              bnd <- make symName
                              pure $ Qual $ QualifiedBinding ns bnd
            _ ->
                throwErrorDebugS $
                "Leading slash expected after `break` in the binding " <> show s
