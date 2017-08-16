

import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import           Test.Hspec



main = hspec $ do
    passesSpec
    DFLowering.smapSpec
    DFLowering.ifSpec
    ALangVerify.currying
