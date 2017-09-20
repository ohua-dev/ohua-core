-- |
-- Module      : $Header$
-- Description : Serialization to JSON for the Ohua types
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.Serialize.JSON (encode, eitherDecode) where


import           Data.Aeson
import           Data.Aeson.Types
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.Types
import Data.List
import Data.Maybe

baseOptions = defaultOptions { unwrapUnaryRecords = True, fieldLabelModifier = camelTo2 '_' }
sourceOptions = baseOptions { constructorTagModifier = f, sumEncoding = TaggedObject "type" "val" }
  where
    f "LocalSource" = "local"
    f "EnvSource" = "env"
    f _ = error "This is only intended for use with something of type `Source`"

operatorOptions = baseOptions
    { fieldLabelModifier =
        fieldLabelModifier baseOptions . fromMaybe (error "no prefix") . stripPrefix "operator"
    }

qualBindOptions = baseOptions
    { fieldLabelModifier =
        fieldLabelModifier baseOptions . fromMaybe (error "no prefix") . stripPrefix "qb"
    }

instance ToJSON Operator where toEncoding = genericToEncoding operatorOptions
instance FromJSON Operator where parseJSON = genericParseJSON operatorOptions
instance ToJSON Target where toEncoding = genericToEncoding baseOptions
instance FromJSON Target where parseJSON = genericParseJSON baseOptions
instance ToJSON a => ToJSON (Arc a) where toEncoding = genericToEncoding baseOptions
instance FromJSON a => FromJSON (Arc a) where parseJSON = genericParseJSON baseOptions
instance ToJSON a => ToJSON (Source a) where toEncoding = genericToEncoding sourceOptions
instance FromJSON a => FromJSON (Source a) where parseJSON = genericParseJSON sourceOptions
instance ToJSON OutGraph where toEncoding = genericToEncoding baseOptions
instance FromJSON OutGraph where parseJSON = genericParseJSON baseOptions
instance ToJSON HostExpr where toEncoding = toEncoding . unwrapHostExpr
instance FromJSON HostExpr where parseJSON = fmap HostExpr . parseJSON
instance ToJSON QualifiedBinding where toEncoding = genericToEncoding qualBindOptions
instance FromJSON QualifiedBinding where parseJSON = genericParseJSON qualBindOptions
instance ToJSON Binding where toEncoding = toEncoding . unBinding
instance FromJSON Binding where parseJSON = fmap Binding . parseJSON
instance ToJSON FnId where toEncoding = toEncoding . unFnId
instance FromJSON FnId where parseJSON = fmap FnId . parseJSON
instance ToJSON NSRef where toEncoding = toEncoding . nsRefToList
instance FromJSON NSRef where parseJSON = fmap nsRefFromList . parseJSON
