

import           PassesSpec
import qualified DFLowering
import           Test.Hspec



main = hspec $ do
    passesSpec
    DFLowering.spec
