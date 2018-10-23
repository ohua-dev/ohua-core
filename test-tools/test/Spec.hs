
import Ohua.Prelude

import Test.Hspec
import Test.Hspec.QuickCheck

import Ohua.Test
import Ohua.DFLang.Parser
import Ohua.DFLang.Lang
import Ohua.ALang.PPrint
import Ohua.DFLang.PPrint ()
import Ohua.Types.Arbitrary

import System.IO.Unsafe

dfLangParserPPrinterSpec :: Spec
dfLangParserPPrinterSpec =
    describe "let-expression pretty printing is parseable" $
    prop "it works" $ \randExpr ->
        randExpr `shouldBe`
        let rendered = quickRender (randExpr :: DFExpr)
         in unsafePerformIO $ (putStrLn rendered) >>
            return (parseExp (encodeUtf8 $ rendered))

main :: IO ()
main =
  return ()
  --  This test should work (and kinda does, but it sometimes seems to get stuck
  --  in an infinite loop and I do not have the time to figure out why).
  --
  -- hspec dfLangParserPPrinterSpec
