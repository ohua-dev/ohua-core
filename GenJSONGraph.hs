import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Ohua.DFGraph
import Ohua.Serialize.JSON
import Test.QuickCheck
import Ohua.Types.Arbitrary


main = do
    l <- generate (arbitrary :: Gen OutGraph)

    print l

    L.writeFile "a-graph.json" $ encode l
