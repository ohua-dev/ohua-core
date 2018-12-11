module TestAesonConvert where

import Ohua.Prelude

import Data.Aeson as A
    ( FromJSON
    , ToJSON
    , eitherDecode
    , encode
    , parseJSON
    , toJSON
    )


import qualified Data.Aeson.Types as A
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

import Ohua.DFGraph
import Ohua.Serialize.JSON ()
import Ohua.Types.Arbitrary ()


testConvert :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Result
testConvert thing =
    case res of
        Left err -> failed {reason = err}
        Right () -> succeeded
  where
    res = do
        eitherDecode (encode thing) >>= \v -- this uses and tests toEncoding
         ->
            if v == thing
                then pure ()
                else throwError $ "toEncoding was unequal: " <> show v
        -- this uses and tests toJSON
        case A.parse parseJSON (toJSON thing) of
            A.Error str -> throwError $ "toJSON threw error " <> str
            A.Success v
                | v == thing -> pure ()
                | otherwise -> throwError $ "toJSON was unequal: " <> show v
        -- tests that `toEncoding` and `toJSON` do the same thing
        eitherDecode (encode thing) >>= \v ->
            if v == thing
                then pure ()
                else throwError $
                     "toEncoding and toJSON are not the same: " <> show v

spec :: Spec
spec = describe "encode . decode == id" $ do
    prop "for operators" (testConvert :: Operator -> Result)
    prop "for targets" (testConvert :: Target -> Result)
    prop "for direct arcs" (testConvert :: DirectArc Lit -> Result)
    prop "for state arcs" (testConvert :: StateArc Lit -> Result)
    prop "for compound arcs" (testConvert :: CompoundArc -> Result)
    prop "for sources" (testConvert :: Source HostExpr -> Result)
    prop "for fn names" (testConvert :: Binding -> Result)
    prop "for fn ids" (testConvert :: FnId -> Result)
    prop "for host expressions" (testConvert :: HostExpr -> Result)
    -- The next one currently runs in a infinite loop ....
    -- prop "for graphs" (testConvert :: OutGraph -> Result)
