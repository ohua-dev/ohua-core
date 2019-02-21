module ControlPassesSpec where



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

spec :: Spec
spec = do
    describe "conditionals" $ do
        it "nested-if" $
            transformInto
                ifRewrite
                [embedALang|
              let cond1 = conditionals.iftest/get_ctrl_input () in
              let cond2 = conditionals.iftest/get_another_ctrl_input () in
              let inp = conditionals.iftest/get_input () in
                ohua.lang/if cond1
                             (\_ ->
                                ohua.lang/if cond2
                                            (\_ -> conditionals.iftest/modify_string_positive inp)
                                            (\ _ -> conditionals.iftest/modify_string_negative inp))
                            (\_ -> inp)
            |]
                [embedALang|
                    let cond1 = conditionals.iftest/get_ctrl_input () in
                    let cond2 = conditionals.iftest/get_another_ctrl_input () in
                    let inp = conditionals.iftest/get_input () in
                    let d =
                        let ctrls_1 = ohua.lang/ifFun cond1 in
                        let ctrlTrue_1 = ohua.lang/nth 0 2 ctrls_1 in
                        let ctrlFalse_1 = ohua.lang/nth 1 2 ctrls_1 in
                        let trueResult_1 =
                            let ctrl_2 = ohua.lang/ctrl ctrlTrue_1 cond2 inp in
                            let cond2_0 = ohua.lang/nth 0 2 ctrl_2 in
                            let inp_2 = ohua.lang/nth 1 2 ctrl_2 in
                            let c =
                                let ctrls_0 = ohua.lang/ifFun cond2_0 in
                                let ctrlTrue_0 = ohua.lang/nth 0 2 ctrls_0 in
                                let ctrlFalse_0 = ohua.lang/nth 1 2 ctrls_0 in
                                let trueResult_0 =
                                    let ctrl_0 = ohua.lang/ctrl ctrlTrue_0 inp_2 in
                                    let inp_0 = ohua.lang/nth 0 1 ctrl_0 in
                                    let a = conditionals.iftest/modify_string_positive inp_0 in a in
                                let falseResult_0 =
                                    let ctrl_1 = ohua.lang/ctrl ctrlFalse_0 inp_2 in
                                    let inp_1 = ohua.lang/nth 0 1 ctrl_1 in
                                    let b = conditionals.iftest/modify_string_negative inp_1 in b in
                                let result_0 =
                                    ohua.lang/select cond2_0 trueResult_0 falseResult_0 in
                                result_0 in
                            c in
                        let falseResult_1 =
                            let ctrl_3 = ohua.lang/ctrl ctrlFalse_1 inp in
                            let inp_3 = ohua.lang/nth 0 1 ctrl_3 in
                            let e = ohua.lang/id inp_3 in e in
                        let result_1 = ohua.lang/select cond1 trueResult_1 falseResult_1 in
                        result_1 in
                    d
                 |]
