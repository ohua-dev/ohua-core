module TestAesonConvert where


import           Data.Aeson
import           Ohua.ALang.Lang
import           Ohua.DFGraph
import           Ohua.Serialize.JSON   ()
import           Ohua.Types
import           Ohua.Types.Arbitrary
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck


testConvert :: (ToJSON a, FromJSON a, Arbitrary a, Show a, Eq a) => a -> Bool
testConvert thing = Just thing == decode (encode thing)



spec :: Spec
spec = describe "encode . decode == id" $ do
    prop "for operators" (testConvert :: Operator -> Bool)
    prop "for targets" (testConvert :: Target -> Bool)
    prop "for arcs" (testConvert :: Arc -> Bool)
    prop "for sources" (testConvert :: Source -> Bool)
    prop "for fn names" (testConvert :: FnName -> Bool)
    prop "for fn ids" (testConvert :: FnId -> Bool)
    prop "for host expressions" (testConvert :: HostExpr -> Bool)
    prop "for graphs" (testConvert :: OutGraph -> Bool)

