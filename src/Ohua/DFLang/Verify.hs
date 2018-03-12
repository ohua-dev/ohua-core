module Ohua.DFLang.Verify
  (verify)
  where



import           Control.Monad.Error.Class hiding (Error)
import           Data.Foldable
import qualified Data.HashMap.Strict       as HM
import           Data.Monoid
import           Lens.Micro.Platform
import           Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs          as Refs
import           Ohua.Types
import           Ohua.Util
import           Ohua.Util.Str             (showS)


verify :: forall m . MonadError Error m => DFExpr -> m ()
verify = sequence_ . sequenceA checks
  where
    checks :: [DFExpr -> m ()]
    checks = [ checkBuiltinArities
             ]


checkBuiltinArities :: MonadError Error m => DFExpr -> m ()
checkBuiltinArities e =
  for_ (letExprs e) $ \LetExpr{ functionRef, callArguments } ->
    case builtinOps ^? ix functionRef of
      Just arity | arity /= length callArguments ->
                   throwErrorS $ "Wrong arity for builtin operator " <> showS functionRef <> " expected " <> showS arity <> " got " <> showS (length callArguments) <> "."
      _ -> pure ()
  where
    builtinOps :: HM.HashMap DFFnRef Int
    builtinOps =
      [ (Refs.oneToN, 2)
      , (Refs.size, 1)
      , (Refs.collect, 2)
      , (Refs.smapFun, 1)
      , (Refs.id, 1)
      , (Refs.bool, 1)
      , (Refs.select, 3)
      , (Refs.seq, 1)
      ]
