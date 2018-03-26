-- |
-- Module      : $Header$
-- Description : Serialization to JSON for the Ohua types
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
--
-- This module basically does not expose anything that is not available otherwise.
-- It is a single place to collect the orphan instances for the types in ohua that are part of the
-- external API.
--
-- The two exposed functions, 'encode' and 'eitherDecode', are just convenient reexports from
-- @Data.Aeson@.

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Ohua.Serialize.JSON (encode, eitherDecode) where

#include "compat.h"

import Data.Aeson
import Data.List
import Data.Maybe
import Ohua.DFGraph
import Ohua.Types hiding (Options)
import qualified Ohua.Util.Str as Str


#if AESON_EXPORTS_OPTIONS
import Data.Aeson.Types
#endif

baseOptions :: Options
baseOptions = defaultOptions
    { unwrapUnaryRecords = True
    , fieldLabelModifier = camelTo2 '_'
    }

sourceOptions :: Options
sourceOptions = baseOptions
    { constructorTagModifier = \case
        "LocalSource" -> "local"
        "EnvSource" -> "env"
        _ -> error "This is only intended for use with something of type `Source`"
    , sumEncoding = TaggedObject "type" "val"
    }

operatorOptions :: Options
operatorOptions = baseOptions
    { fieldLabelModifier =
        fieldLabelModifier baseOptions . fromMaybe (error "no prefix") . stripPrefix "operator"
    }

qualBindOptions :: Options
qualBindOptions = baseOptions
    { fieldLabelModifier =
        fieldLabelModifier baseOptions . fromMaybe (error "no prefix") . stripPrefix "qb"
    }

instance ToJSON Str.Str where
    toJSON = toJSON . Str.toString
    toEncoding = toEncoding . Str.toString
instance FromJSON Str.Str where parseJSON = fmap Str.fromString . parseJSON
instance ToJSON Operator where
    toEncoding = genericToEncoding operatorOptions
    toJSON = genericToJSON operatorOptions
instance FromJSON Operator where
    parseJSON = genericParseJSON operatorOptions
instance ToJSON Target where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON Target where
    parseJSON = genericParseJSON baseOptions
instance ToJSON a => ToJSON (Arc a) where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON a => FromJSON (Arc a) where
    parseJSON = genericParseJSON baseOptions
instance ToJSON a => ToJSON (Source a) where
    toEncoding = genericToEncoding sourceOptions
    toJSON = genericToJSON sourceOptions
instance FromJSON a => FromJSON (Source a) where
    parseJSON = genericParseJSON sourceOptions
instance ToJSON OutGraph where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON OutGraph where
    parseJSON = genericParseJSON baseOptions
instance ToJSON HostExpr where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON HostExpr where
    parseJSON = genericParseJSON baseOptions
instance ToJSON QualifiedBinding where
    toEncoding = genericToEncoding qualBindOptions
    toJSON = genericToJSON qualBindOptions
instance FromJSON QualifiedBinding where
    parseJSON = genericParseJSON qualBindOptions
instance ToJSON Binding where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON Binding where
    parseJSON = genericParseJSON baseOptions
instance ToJSON FnId where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON FnId where
    parseJSON = genericParseJSON baseOptions
instance ToJSON NSRef where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
instance FromJSON NSRef where
    parseJSON = genericParseJSON baseOptions
