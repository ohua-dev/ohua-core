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
import           Lens.Micro
import           Ohua.LensClasses

newtype FnId = FnId { unFnId :: Int } deriving (Eq, Ord)

instance Show FnId where
    show = show . unFnId
instance Hashable FnId where hashWithSalt s = hashWithSalt s . unFnId
instance NFData FnId where rnf (FnId i) = rnf i

newtype Binding = Binding { unBinding :: String }
    deriving (Eq, Hashable)

instance Show Binding where show = unBinding
instance NFData Binding where rnf (Binding b) = rnf b


instance IsString Binding where
    fromString = either error (either (const $ error "Binding must not be fully qualified") id) . symbolFromString

data FnName = FnName
    { fnNameNamespace :: !String
    , fnNameName      :: !String
    } deriving (Eq, Ord)

instance HasName FnName String where name = lens fnNameName (\s a -> s {fnNameName=a})
instance NFData FnName where rnf (FnName ns n) = rnf ns `seq` rnf n

instance HasNamespace FnName String where namespace = lens fnNameNamespace (\s a -> s {fnNameNamespace=a})

instance Show FnName where
    show n
        | null (n^.name) = n^.namespace
        | otherwise = n^.namespace ++ "/" ++ n^.name

instance IsString FnName where
    fromString = either error (either id (const $ error "Function name must be fully qualified")) . symbolFromString

symbolFromString :: String -> Either String (Either FnName Binding)
symbolFromString [] = Left "Symbols cannot be empty"
symbolFromString s =
    case break (== '/') s of
        ([], _) -> Left "Unexpected '/' at start"
        (name,[]) -> Right $ Right $ Binding name
        (ns, '/':name)
            | '/' `elem` name -> Left "Too many '/' delimiters found."
            | otherwise -> Right $ Left $ FnName ns name


class ExtractBindings a where
    extractBindings :: a -> [Binding]

