
import Data.Aeson
import GHC.Generics

newtype MyType = MyType { aField :: Int } deriving (Eq, Show, Generic)


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
        asJSON = encode example
     in do print asJSON
           case eitherDecode asJSON of
               Left err -> putStrLn err
               Right v
                   | v == example -> pure ()
                   | otherwise -> print v
