{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.Aeson
import GHC.Generics

data MyType = MyType Int deriving (Eq, Show, Generic)


baseOptions :: Options
baseOptions =
    defaultOptions
        {unwrapUnaryRecords = True, fieldLabelModifier = camelTo2 '_'}


instance ToJSON MyType where
    toJSON = genericToJSON baseOptions
    toEncoding = genericToEncoding baseOptions

instance FromJSON MyType where
    parseJSON = genericParseJSON baseOptions


main =
    let example = MyType 0
        asJSON = encode $ toJSON example
     in do print asJSON
           print (eitherDecode "[0]" :: Either String MyType)
           case eitherDecode asJSON of
               Left err -> putStrLn err
               Right v
                   | v == example -> pure ()
                   | otherwise -> print v
