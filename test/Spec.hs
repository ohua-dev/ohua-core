
import Ohua.Prelude

-- import qualified ALangVerify
import qualified DFLowering
import           PassesSpec
import           Test.Hspec
import qualified TestAesonConvert
import qualified TestALangUtils



main :: IO ()
main = hspec $ do
    passesSpec
    DFLowering.generalLowering
    DFLowering.ifSpec
    DFLowering.seqSpec
    DFLowering.smapSpec
    --ALangVerify.currying
    TestALangUtils.spec
    TestAesonConvert.spec
