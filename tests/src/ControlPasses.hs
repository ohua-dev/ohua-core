module ControlPasses where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.PPrint (quickRender)
import Ohua.ALang.Passes (normalize)
import Ohua.ALang.Passes.If (ifRewrite)

import Ohua.Test (embedALang, showWithPretty)

import Test.Hspec

runPass pass expr = runSilentLoggingT $ runFromExpr def pass expr

transformInto pass expr expected =
    runPass (normalize >=> pass) expr >>=
    ((`shouldBe` Right (showWithPretty expected)) . fmap showWithPretty)

passesSpec :: Spec
passesSpec = do
    describe "conditionals" $ do
        it "nested-if" $
            transformInto
                ifRewrite
                [embedALang|
              let ctrl = conditionals.iftest/get_ctrl_input () in
              let another_ctrl = conditionals.iftest/get_another_ctrl_input () in
              let inp = conditionals.iftest/get_input () in
                ohua.lang/if ctrl
                             (\_ ->
                                ohua.lang/if another_ctrl
                                            (\_ -> conditionals.iftest/modify_string_positive inp)
                                            (\ _ -> conditionals.iftest/modify_string_negative inp))
                            (\_ -> inp)
            |]
                [embedALang|
                     let ctrl = conditionals.iftest/get_ctrl_input () in
                     let another_ctrl = conditionals.iftest/get_another_ctrl_input () in
                     let inp = conditionals.iftest/get_input () in
                     let d =
                         let ctrls_1 = ohua.lang/ifFun ctrl in
                         let ctrlTrue_1 = ohua.lang/nth 0 2 ctrls_1 in
                         let ctrlFalse_1 = ohua.lang/nth 1 2 ctrls_1 in
                         let trueResult_1 =
                             let ctrl_2 =
                                 ohua.lang/ctrl ctrlTrue_1 another_ctrl inp 0 2 1 2 0 1 0 1 in
                             let another_ctrl_0 = ohua.lang/nth 0 10 ctrl_2 in
                             let inp_2 = ohua.lang/nth 1 10 ctrl_2 in
                             let lit_0_0 = ohua.lang/nth 2 10 ctrl_2 in
                             let lit_2_0 = ohua.lang/nth 3 10 ctrl_2 in
                             let lit_1_0 = ohua.lang/nth 4 10 ctrl_2 in
                             let lit_2_1 = ohua.lang/nth 5 10 ctrl_2 in
                             let lit_0_1 = ohua.lang/nth 6 10 ctrl_2 in
                             let lit_1_1 = ohua.lang/nth 7 10 ctrl_2 in
                             let lit_0_2 = ohua.lang/nth 8 10 ctrl_2 in
                             let lit_1_2 = ohua.lang/nth 9 10 ctrl_2 in
                             let c =
                                 let ctrls_0 = ohua.lang/ifFun another_ctrl_0 in
                                 let ctrlTrue_0 = ohua.lang/nth lit_0_0 lit_2_0 ctrls_0 in
                                 let ctrlFalse_0 = ohua.lang/nth lit_1_0 lit_2_0 ctrls_0 in
                                 let trueResult_0 =
                                     let ctrl_0 = ohua.lang/ctrl ctrlTrue_0 inp_2 in
                                     let inp_0 = ohua.lang/nth lit_0_0 lit_1_0 ctrl_0 in
                                     let a = conditionals.iftest/modify_string_positive inp_0 in a in
                                 let falseResult_0 =
                                     let ctrl_1 = ohua.lang/ctrl ctrlFalse_0 inp_2 in
                                     let inp_1 = ohua.lang/nth lit_0_0 lit_1_0 ctrl_1 in
                                     let b = conditionals.iftest/modify_string_negative inp_1 in b in
                                 let result_0 =
                                     ohua.lang/select another_ctrl_0 trueResult_0 falseResult_0 in
                                 result_0 in
                             c in
                         let falseResult_1 =
                             let ctrl_3 = ohua.lang/ctrl ctrlFalse_1 inp in
                             let inp_3 = ohua.lang/nth 0 1 ctrl_3 in
                             let e = ohua.lang/id inp_3 in e in
                         let result_1 = ohua.lang/select ctrl trueResult_1 falseResult_1 in
                         result_1 in
                     d
                    |]
