import Ohua.Prelude

import qualified DFLowering
import qualified DFOptimizations

import qualified ControlPasses
import qualified DFPassesSpec
import PassesSpec
import qualified TailRecSpec
import Test.Hspec
import qualified TestALangUtils
import qualified TestAesonConvert

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
        ControlPasses.passesSpec
