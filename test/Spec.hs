

import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import           Test.Hspec
import qualified TestALangUtils



main = hspec $ do
    passesSpec
    DFLowering.smapSpec
    DFLowering.ifSpec
    DFLowering.generalLowering
    DFLowering.seqSpec
    DFLowering.recurSpec
    ALangVerify.currying
    TestALangUtils.spec
