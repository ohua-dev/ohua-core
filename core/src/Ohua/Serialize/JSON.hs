-- |
-- Module      : $Header$
-- Description : Serialization to JSON for the Ohua types
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
--
-- This module basically does not expose anything that is not available
-- otherwise. It is a single place to collect the orphan instances for the types
-- in ohua that are part of the external API.
--
-- The two exposed functions, 'encode' and 'eitherDecode', are just convenient reexports from
-- @Data.Aeson@.
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ohua.Serialize.JSON
    ( encode
    , eitherDecode
    ) where

import Ohua.Prelude hiding (Options)

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Data.List (stripPrefix)

import Ohua.DFGraph
import Ohua.DFLang.Lang (NodeType(..))

import Data.Generics.Product

baseOptions :: Options
baseOptions =
    defaultOptions
        {unwrapUnaryRecords = True, fieldLabelModifier = camelTo2 '_'}

sourceOptions :: Options
sourceOptions =
    baseOptions
        { constructorTagModifier =
              \case
                  "LocalSource" -> "local"
                  "EnvSource" -> "env"
        }

operatorOptions :: Options
operatorOptions =
    baseOptions
        { fieldLabelModifier =
              fieldLabelModifier baseOptions .
              fromMaybe (error "no prefix") . stripPrefix "operator"
        }

-- ############## Begin Disgusting Workaround ##############

-- This is because we need 'FunRef' literals as env inputs sometimes and rust
-- does not like the default way that a 'FunRef' with a 'Nothing' value as id is
-- serialized. So we need this until we move the function id to some other
-- place.

data PrivateFunRefLitWrapper
    = FunRefLit QualifiedBinding
    | OtherLit Lit
    deriving (Generic)

instance ToJSON PrivateFunRefLitWrapper where
    toJSON (Ohua.Serialize.JSON.FunRefLit l) =
        genericToJSON baseOptions {unwrapUnaryRecords = False} l
    toJSON (OtherLit o) = toJSON o

instance FromJSON PrivateFunRefLitWrapper where
    parseJSON v =
        (do l@Ohua.Serialize.JSON.FunRefLit {} <-
                genericParseJSON baseOptions {unwrapUnaryRecords = False} v
            pure l) <|>
        (OtherLit <$> parseJSON v)

-- ############## End Disgusting Workaround ##############

makeInJSON :: Make t => SourceType t -> Parser t
makeInJSON = either (fail . toString) pure . make

unwrapToEncoding :: (Unwrap t, ToJSON (SourceType t)) => t -> Encoding
unwrapToEncoding = toEncoding . unwrap

unwrapToJSON :: (Unwrap t, ToJSON (SourceType t)) => t -> Value
unwrapToJSON = toJSON . unwrap

makeParseJSON :: (Make t, FromJSON (SourceType t)) => Value -> Parser t
makeParseJSON = makeInJSON <=< parseJSON

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

instance (ToJSON a, ToJSON b) => ToJSON (Arc a b) where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions

instance (FromJSON a, FromJSON b) => FromJSON (Arc a b) where
    parseJSON = genericParseJSON baseOptions

instance ToJSON NodeType where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions

instance FromJSON NodeType where
    parseJSON = genericParseJSON baseOptions

instance ToJSON a => ToJSON (Source a) where
    toEncoding = genericToEncoding sourceOptions
    toJSON = genericToJSON sourceOptions

instance FromJSON a => FromJSON (Source a) where
    parseJSON = genericParseJSON sourceOptions

mapArcsLit :: Traversal (Arcs a) (Arcs b) a b
mapArcsLit f (Arcs a b c) = Arcs <$> envSource f a <*> envSource f b <*> pure c
  where
    envSource :: Traversal [Arc a (Source b)] [Arc a (Source c)] b c
    envSource = traverse @[] . param @0 . param @0

mapOutGraphLit :: Traversal (AbstractOutGraph a) (AbstractOutGraph b) a b
mapOutGraphLit = field @"arcs" . mapArcsLit

instance ToJSON OutGraph where
    toJSON =
        genericToJSON baseOptions .
        (mapOutGraphLit %~ (\case
             Ohua.Prelude.FunRefLit (FunRef l _) ->
                 Ohua.Serialize.JSON.FunRefLit l
             other -> OtherLit other))

instance FromJSON OutGraph where
    parseJSON =
        fmap
            (mapOutGraphLit %~ \case
                 Ohua.Serialize.JSON.FunRefLit l ->
                     Ohua.Prelude.FunRefLit (FunRef l Nothing)
                 OtherLit l -> l) .
        genericParseJSON baseOptions

instance ToJSON HostExpr where
    toEncoding = unwrapToEncoding
    toJSON = unwrapToJSON

instance FromJSON HostExpr where
    parseJSON = makeParseJSON

instance ToJSON FunRef where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions

instance FromJSON FunRef where
    parseJSON = genericParseJSON baseOptions

instance ToJSON Lit where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions

instance FromJSON Lit where
    parseJSON = genericParseJSON baseOptions

instance ToJSON QualifiedBinding where
    toJSON b = object ["namespace" .= (b ^. namespace), "name" .= (b ^. name)]

instance FromJSON QualifiedBinding where
    parseJSON =
        withObject "Expected object" $ \o ->
            QualifiedBinding <$> o .: "namespace" <*> o .: "name"

instance ToJSON Binding where
    toEncoding = unwrapToEncoding
    toJSON = unwrapToJSON

instance FromJSON Binding where
    parseJSON = makeParseJSON

instance ToJSON FnId where
    toEncoding = unwrapToEncoding
    toJSON = unwrapToJSON

instance FromJSON FnId where
    parseJSON = makeParseJSON

instance ToJSON NSRef where
    toEncoding = unwrapToEncoding
    toJSON = unwrapToJSON

instance FromJSON NSRef where
    parseJSON = makeParseJSON

instance FromJSON a => FromJSON (Arcs a) where
    parseJSON = genericParseJSON baseOptions

instance ToJSON a => ToJSON (Arcs a) where
    toEncoding = genericToEncoding baseOptions
    toJSON = genericToJSON baseOptions
