module DFOptimizations where

import Ohua.Prelude

import Test.Hspec

import Ohua.DFLang.Lang
import Ohua.DFLang.PPrint
import Ohua.DFLang.Passes
import Ohua.Test (embedDFLang, showWithPretty)
import Ohua.Test.DFGraph
import qualified Ohua.DFLang.Refs as Refs

optimizeCtrl = collapseNth (== nodeRef Refs.ctrl)

shouldOptimizeTo :: DFExpr -> DFExpr -> Expectation
shouldOptimizeTo input expected = do
    let optimized = optimizeCtrl input
    -- traceM $ "expected:\n" <> (show $ prettyDFExpr expected)
    -- traceM $ "got:\n" <> (show $ prettyDFExpr optimized)
    showWithPretty optimized `shouldBe` showWithPretty expected

optimizeAndValidate :: DFExpr -> DFExpr -> String -> Spec
optimizeAndValidate sourceExpr targetExpr statementType = do
    it ("correctly optimizes an " ++ statementType ++ " statement") $
        sourceExpr `shouldOptimizeTo` targetExpr

optimizations :: Spec
optimizations =
    describe "optimize ctrl constructs" $ do
        let sourceExpr =
                [embedDFLang|
                  let (ctrl_0) = iftest/get_ctrl_input<1> (()) in
                  let (inp_0) = iftest/get_input<2> (()) in
                  let (ctrls_0) = dataflow ohua.lang/ifFun<3> (ctrl_0) in
                  let (ctrlTrue_0) = ohua.lang/nth<4> (0, 2, ctrls_0) in
                  let (ctrlFalse_0) = ohua.lang/nth<5> (1, 2, ctrls_0) in
                  let (ctrl_1) = dataflow ohua.lang/ctrl<6> (ctrlTrue_0, inp_0) in
                  let (inp_0_0) = ohua.lang/nth<7> (0, 1, ctrl_1) in
                  let (a) = iftest/modify_string_positive<8> (inp_0_0) in
                  let (ctrl_2) = dataflow ohua.lang/ctrl<9> (ctrlFalse_0, inp_0) in
                  let (inp_0_1) = ohua.lang/nth<10> (0, 1, ctrl_2) in
                  let (b) = iftest/modify_string_negative<11> (inp_0_1) in
                  let (result_0) = dataflow ohua.lang/select<12> (ctrl_0, a, b) in
                  result_0
                |]
        let targetExpr =
                [embedDFLang|
                  let (ctrl_0) = iftest/get_ctrl_input<1> (()) in
                  let (inp_0) = iftest/get_input<2> (()) in
                  let (ctrls_0) = dataflow ohua.lang/ifFun<3> (ctrl_0) in
                  let (ctrlTrue_0) = ohua.lang/nth<4> (0, 2, ctrls_0) in
                  let (ctrlFalse_0) = ohua.lang/nth<5> (1, 2, ctrls_0) in
                  let (inp_0_0) = dataflow ohua.lang/ctrl<6> (ctrlTrue_0, inp_0) in
                  let (a) = iftest/modify_string_positive<8> (inp_0_0) in
                  let (inp_0_1) = dataflow ohua.lang/ctrl<9> (ctrlFalse_0, inp_0) in
                  let (b) = iftest/modify_string_negative<11> (inp_0_1) in
                  let (result_0) = dataflow ohua.lang/select<12> (ctrl_0, a, b) in
                  result_0
                |]
        optimizeAndValidate sourceExpr targetExpr "ctrl"
