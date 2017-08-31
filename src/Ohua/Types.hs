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
import           Data.String
import           GHC.Exts
import           Lens.Micro
import           Ohua.LensClasses
import           Ohua.Util
import qualified Data.Text as T
import Data.Monoid


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
    fromString = either error (either (const $ error "Binding must not be fully qualified") id) . symbolFromString

data FnName = FnName
    { fnNameNamespace :: !T.Text
    , fnNameName      :: !T.Text
    } deriving (Eq, Ord)

instance HasName FnName T.Text where name = lens fnNameName (\s a -> s {fnNameName=a})
instance NFData FnName where rnf (FnName ns n) = rnf ns `seq` rnf n
instance Hashable FnName where hashWithSalt s (FnName a b) = hashWithSalt s (a, b)

instance HasNamespace FnName T.Text where namespace = lens fnNameNamespace (\s a -> s {fnNameNamespace=a})

instance Show FnName where
    show n
        | T.null (n^.name) = T.unpack $ n^.namespace
        | otherwise = T.unpack $ n^.namespace <> "/" <> n^.name

instance IsString FnName where
    fromString = either error (either id (const $ error "Function name must be fully qualified")) . symbolFromString

symbolFromString :: String -> Either String (Either FnName Binding)
symbolFromString [] = Left "Symbols cannot be empty"
symbolFromString s =
    case break (== '/') s of
        ([], _) -> Left "Unexpected '/' at start"
        (name,[]) -> Right $ Right $ Binding  $ T.pack name
        (ns, '/':name)
            | '/' `elem` name -> Left "Too many '/' delimiters found."
            | otherwise -> Right $ Left $ FnName (T.pack ns) (T.pack name)
        _ -> error "Leading slash expected after `break`"


class ExtractBindings a where
    extractBindings :: a -> [Binding]
instance ExtractBindings a => ExtractBindings [a] where extractBindings = concatMap extractBindings


data Assignment
    = Direct !Binding
    | Recursive !Binding
    | Destructure ![Binding]
    deriving (Eq)

instance Show Assignment where
    show (Direct b)      = "Direct " ++ show b
    show (Destructure d) = show d
    show (Recursive b)   = "Recursive " ++ show b

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
    extractBindings (Recursive bnd) = [bnd]

instance NFData Assignment where
    rnf (Destructure ds) = rnf ds
    rnf (Direct d)       = rnf d
    rnf (Recursive d)    = rnf d

_Direct :: Prism' Assignment Binding
_Direct = prism' Direct $ \case { Direct a -> Just a; _ -> Nothing }

_Destructure :: Prism' Assignment [Binding]
_Destructure = prism' Destructure $ \case { Destructure a -> Just a; _ -> Nothing }


flattenAssign :: Assignment -> [Binding]
flattenAssign = extractBindings
