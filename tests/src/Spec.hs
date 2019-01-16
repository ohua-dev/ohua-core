import Ohua.Prelude

import qualified DFLowering
import qualified DFOptimizations

import qualified TailRecSpec
-- FIXME
import PassesSpec
import Test.Hspec
import qualified TestALangUtils
import qualified TestAesonConvert
import qualified DFPassesSpec

main :: IO ()
main =
    hspec $ do
        passesSpec
        DFLowering.generalLowering
        DFLowering.ifSpec
        DFLowering.seqSpec
        DFLowering.smapSpec
        DFOptimizations.optimizations
        TestALangUtils.spec
        TestAesonConvert.spec
        TailRecSpec.passesSpec
        DFPassesSpec.spec
