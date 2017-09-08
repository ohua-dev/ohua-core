-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ohua.Types where

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.HashSet     as HS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text        as T
import           GHC.Exts
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util


newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord)

-- Only here so we can write literals and have them convert automatically
instance Num FnId where
    fromInteger = FnId . fromInteger

instance Show FnId where
    show = show . unFnId
instance Hashable FnId where hashWithSalt s = hashWithSalt s . unFnId
instance NFData FnId where rnf (FnId i) = rnf i

newtype Binding = Binding { unBinding :: T.Text }
    deriving (Eq, Hashable)

instance Show Binding where show = T.unpack . unBinding
instance NFData Binding where rnf (Binding b) = rnf b


instance IsString Binding where
    fromString = Binding . fromString

newtype FnName = FnName { unwrapFnName :: T.Text } deriving (Eq, Ord)

instance HasName FnName T.Text where name = lens unwrapFnName (const FnName)
instance NFData FnName where rnf (FnName n) = rnf n
instance Hashable FnName where hashWithSalt s (FnName n) = hashWithSalt s n

instance Show FnName where
    show = T.unpack . unwrapFnName

instance IsString FnName where
    fromString = either error (either id (const $ error "Function name must be fully qualified")) . symbolFromString . fromString

symbolFromString :: T.Text -> Either String (Either FnName Binding)
symbolFromString s | T.null s = Left "Symbols cannot be empty"
                   | otherwise =
    case T.break (== '/') s of
        (name, ns) | T.null name -> Left "Unexpected '/' at start"
                   | T.null ns -> Right $ Right $ Binding name
        (ns, rest) | Just ('/', name) <- T.uncons rest ->
            if '/' `textElem` name then
                Left "Too many '/' delimiters found."
            else
                Right $ Left $ FnName s
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
