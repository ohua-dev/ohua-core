

import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import           Test.Hspec
import qualified TestAesonConvert
import qualified TestALangUtils
import qualified StrTest



main = hspec $ do
--    passesSpec
--    DFLowering.smapSpec
    StrTest.strTest
    -- DFLowering.ifSpec
    -- DFLowering.generalLowering
    -- DFLowering.seqSpec
    -- ALangVerify.currying
    -- TestALangUtils.spec
    -- TestAesonConvert.spec
