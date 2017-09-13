-- |
-- Module      : $Header$
-- Description : Serialization to JSON for the Ohua types
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.Serialize.JSON where


import           Data.Aeson
import           Data.Aeson.Types
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.Types

myOpts = defaultOptions { unwrapUnaryRecords = True }
sourceOpts = myOpts { constructorTagModifier = f, sumEncoding = TaggedObject "type" "val" }
  where
    f "LocalSource" = "local"
    f "EnvSource" = "env"
    f _ = error "This is only intended for use with something of type `Source`"

instance ToJSON Operator where toEncoding = genericToEncoding myOpts
instance FromJSON Operator where parseJSON = genericParseJSON myOpts
instance ToJSON Target where toEncoding = genericToEncoding myOpts
instance FromJSON Target where parseJSON = genericParseJSON myOpts
instance ToJSON Arc where toEncoding = genericToEncoding myOpts
instance FromJSON Arc where parseJSON = genericParseJSON myOpts
instance ToJSON Source where toEncoding = genericToEncoding sourceOpts
instance FromJSON Source where parseJSON = genericParseJSON sourceOpts
instance ToJSON OutGraph where toEncoding = genericToEncoding myOpts
instance FromJSON OutGraph where parseJSON = genericParseJSON myOpts
instance ToJSON HostExpr where toEncoding = genericToEncoding myOpts
instance FromJSON HostExpr where parseJSON = genericParseJSON myOpts
instance ToJSON QualifiedBinding where toEncoding = genericToEncoding myOpts
instance FromJSON QualifiedBinding where parseJSON = genericParseJSON myOpts
instance ToJSON Binding where toEncoding = genericToEncoding myOpts
instance FromJSON Binding where parseJSON = genericParseJSON myOpts
instance ToJSON FnId where toEncoding = genericToEncoding myOpts
instance FromJSON FnId where parseJSON = genericParseJSON myOpts
instance ToJSON NSRef where toEncoding = genericToEncoding myOpts
instance FromJSON NSRef where parseJSON = genericParseJSON myOpts
