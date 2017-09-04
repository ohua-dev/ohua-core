module ALangVerify where

import           Control.Arrow
import           Control.Monad.Except
import           Data.Function
import           Data.Functor.Identity
import qualified Data.Map.Strict       as Map
import           Data.Maybe
import           Debug.Trace
import           Ohua.ALang.Lang
import           Ohua.ALang.Passes
import           Ohua.Monad
import           Ohua.Types
import           Test.Hspec

-- OhuaT Expr (Either [Char] Expression)
justTrace :: Show e => e -> Expectation
justTrace e = traceShow e $ return ()

--f :: Expression -> OhuaT m a

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
  let f :: Expression -> OhuaT (Either String) Expression
      f = normalize
  let runALangTransforms = either error id . runOhuaT f
  let output = runALangTransforms sourceExpr
  it "just some output" $ justTrace output
