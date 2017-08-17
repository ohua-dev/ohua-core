

import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import           Test.Hspec
import qualified TestALangUtils



main = hspec $ do
    passesSpec
    DFLowering.smapSpec
    DFLowering.ifSpec
    ALangVerify.currying
    TestALangUtils.spec
