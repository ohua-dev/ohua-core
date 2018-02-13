import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Ohua.DFGraph
import           Ohua.Serialize.JSON  ()
import           Ohua.Types.Arbitrary ()
import           Test.QuickCheck


main :: IO ()
main = do
    l <- generate (arbitrary :: Gen OutGraph)

    print l

    L.writeFile "a-graph.json" $ encode l
