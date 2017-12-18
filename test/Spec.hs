

import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import qualified StrTest
import           Test.Hspec
import qualified TestAesonConvert
import qualified TestALangUtils



main = hspec $ do
    passesSpec
    StrTest.strTest
    DFLowering.generalLowering
    DFLowering.ifSpec
    DFLowering.seqSpec
    DFLowering.smapSpec
    ALangVerify.currying
    TestALangUtils.spec
    TestAesonConvert.spec
