module TestAesonConvert where


import           Data.Aeson            as A
import           Data.Aeson.Types      as A
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.Serialize.JSON   ()
import           Ohua.Types
import           Ohua.Types.Arbitrary  ()
import           Test.Hspec
import           Test.Hspec.QuickCheck


testConvert :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
testConvert thing =
    Just thing == decode (encode thing) -- this uses and tests toEncoding
    && parse parseJSON (toJSON thing) == A.Success thing -- this uses and tests toJSON
    && decode (encode thing) == Just (toJSON thing) -- tests that `toEncoding` and `toJSON` do the same thing


spec :: Spec
spec = describe "encode . decode == id" $ do
    prop "for operators" (testConvert :: Operator -> Bool)
    prop "for targets" (testConvert :: Target -> Bool)
    prop "for arcs" (testConvert :: Arc HostExpr -> Bool)
    prop "for sources" (testConvert :: Source HostExpr -> Bool)
    prop "for fn names" (testConvert :: FnName -> Bool)
    prop "for fn ids" (testConvert :: FnId -> Bool)
    prop "for host expressions" (testConvert :: HostExpr -> Bool)
    prop "for graphs" (testConvert :: OutGraph -> Bool)
