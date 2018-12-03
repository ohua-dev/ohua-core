import Ohua.Prelude

-- import qualified ALangVerify
import qualified DFLowering

-- import qualified TailRecSpec
-- FIXME
-- import           PassesSpec
import Test.Hspec
import qualified TestALangUtils
import qualified TestAesonConvert

main :: IO ()
main =
    hspec $
       -- FIXME
       -- passesSpec
     do
        DFLowering.generalLowering
        DFLowering.ifSpec
          -- FIXME
        -- DFLowering.seqSpec
        -- DFLowering.smapSpec
    --ALangVerify.currying
        TestALangUtils.spec
        TestAesonConvert.spec
        -- TailRecSpec.passesSpec
