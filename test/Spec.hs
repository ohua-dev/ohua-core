

import qualified DFLowering
import           PassesSpec
import qualified ALangVerify
import           Test.Hspec



main = hspec $ do
    passesSpec
    DFLowering.smapSpec
    DFLowering.ifSpec
    ALangVerify.currying
