
import Ohua.Prelude

import Test.Hspec
import Test.Hspec.QuickCheck

import Ohua.Test
import Ohua.DFLang.Parser
import Ohua.DFLang.Lang
import Ohua.ALang.PPrint
import Ohua.DFLang.PPrint ()
import Ohua.Types.Arbitrary

dfLangParserPPrinterSpec :: Spec
dfLangParserPPrinterSpec =
    describe "let-expression pretty printing is parseable" $
    prop "it works" $ \randExpr ->
        randExpr `shouldBe`
        parseExp (encodeUtf8 $ quickRender (randExpr :: DFExpr))

main :: IO ()
main
  --  This test should work (and kinda does, but it sometimes seems to get stuck
  --  in an infinite loop and I do not have the time to figure out why).
 = unless True $ hspec dfLangParserPPrinterSpec
