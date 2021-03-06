module Ohua.DFLang.Verify
    ( verify
    ) where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM

import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs

verify ::
       forall m. MonadError Error m
    => DFExpr
    -> m ()
verify = sequence_ . sequenceA checks
  where
    checks :: [DFExpr -> m ()]
    checks = [checkBuiltinArities]

checkBuiltinArities :: MonadError Error m => DFExpr -> m ()
checkBuiltinArities e =
    for_ (letExprs e) $ \LetExpr {functionRef, callArguments} ->
        case HM.lookup functionRef builtinOps of
            Just arity
                | arity /= length callArguments ->
                    throwErrorS $
                    "Wrong arity for builtin operator " <> show functionRef <>
                    " expected " <>
                    show arity <>
                    " got " <>
                    show (length callArguments) <>
                    "."
            _ -> pure ()
  where
    builtinOps :: HM.HashMap DFFnRef Int
    builtinOps = [(Refs.collect, 2), (Refs.smapFun, 1), (Refs.select, 3)]
