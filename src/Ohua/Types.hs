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
import           Data.Hashable
import qualified Data.HashSet     as HS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text        as T
import qualified Data.Vector      as V
import           GHC.Exts
import           GHC.Generics
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util


newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord, Generic)

-- Only here so we can write literals and have them convert automatically
instance Num FnId where
    fromInteger = FnId . fromInteger

instance Show FnId where
    show = show . unFnId
instance Hashable FnId where hashWithSalt s = hashWithSalt s . unFnId
instance NFData FnId where rnf (FnId i) = rnf i

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

newtype NSRef = NSRef { unwrapNSRef :: V.Vector Binding }
    deriving (Eq, Generic)

instance Show NSRef where
    show = T.unpack . T.intercalate "." . map unBinding . nsRefToList

nsRefFromList :: [Binding] -> NSRef
nsRefFromList = NSRef . V.fromList


nsRefToList :: NSRef -> [Binding]
nsRefToList = V.toList . unwrapNSRef

instance Hashable NSRef where
    hashWithSalt salt = hashWithSalt salt . nsRefToList
    {-# INLINE hashWithSalt #-}

instance NFData NSRef where
    rnf (NSRef a) = rnf a

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

data SomeBinding
    = Unqual Binding
    | Qual QualifiedBinding
    deriving (Eq, Show)

instance Hashable SomeBinding where
    hashWithSalt s (Unqual b) = hashWithSalt s (0 :: Int, b)
    hashWithSalt s (Qual b)   = hashWithSalt s (1 :: Int, b)

instance IsString SomeBinding where
    fromString = either error id . symbolFromString . T.pack

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


data Assignment
    = Direct !Binding
    | Destructure ![Binding]
    deriving (Eq)

instance Show Assignment where
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

instance NFData Assignment where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d)       = rnf d

_Direct :: Prism' Assignment Binding
_Direct = prism' Direct $ \case { Direct a -> Just a; _ -> Nothing }

_Destructure :: Prism' Assignment [Binding]
_Destructure = prism' Destructure $ \case { Destructure a -> Just a; _ -> Nothing }


flattenAssign :: Assignment -> [Binding]
flattenAssign = extractBindings


type Error = T.Text
type Warning = T.Text
type Warnings = [Warning]


data CompilerState = CompilerState !NameGenerator !Int
data CompilerEnv

nameGenerator :: Lens' CompilerState NameGenerator
nameGenerator f (CompilerState gen counter) = flip CompilerState counter <$> f gen

idCounter :: Lens' CompilerState Int
idCounter f (CompilerState gen counter) = CompilerState gen <$> f counter

data NameGenerator = NameGenerator !(HS.HashSet Binding) [Binding]

takenNames :: Lens' NameGenerator (HS.HashSet Binding)
takenNames f (NameGenerator taken l) = flip NameGenerator l <$> f taken

simpleNameList :: Lens' NameGenerator [Binding]
simpleNameList f (NameGenerator taken l) = NameGenerator taken <$> f l
