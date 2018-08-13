module ALangVerify where

import Protolude

import Data.Default.Class
import Test.Hspec

import Ohua.ALang.Lang
import Ohua.ALang.Passes
import Ohua.Monad

-- OhuaT Expr (Either [Char] Expression)
justTrace :: Show e => IO e -> Expectation
justTrace e = e >>= print

-- f :: Expression -> OhuaT m a

currying :: Spec
currying = describe "resolution of curried functions" $ do
  let sourceExpr =
          Let "a" ("com.ohua.lang/id" `Apply` (Var $ Env 0)) $
          Let "b" ("com.ohua.lang/id" `Apply` (Var $ Env 1)) $
          Let "c" ("com.ohua.lang/id" `Apply` (Var $ Env 2)) $
          Let "f" (Apply "some-ns/fn-with-3-args" "a") $
          Let "f'" (Apply "f" "b") $
          Let "x" (Apply "f'" "c")
          "x"
  let f = normalize
  let runALangTransforms = fmap (either panic identity) . runSilentLoggingT . runFromExpr def f
  let output = runALangTransforms sourceExpr
  it "just some output" $ justTrace output
