module Ohua.DFLang.HOF.Seq where

import           Data.Sequence        as S
import           Ohua.DFLang.HOF
import           Ohua.Types
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Control.Monad.Except
import           Control.Monad.State

data SeqFn = SeqFn {
  before :: DFVar,
  afterExpr :: Lambda
}

instance HigherOrderFunction SeqFn where
  name = "com.ohua.lang/seq"

  parseCallAndInitState [Variable before, LamArg after] = return $ SeqFn before after
  parseCallAndInitState as = throwError "seq not defined for arguments: " -- TODO ++ show as

  createContextEntry = return S.empty

  createContextExit assignment = return S.empty

  scopeFreeVariables lam freeVars = return (S.empty, [])

  contextifyUnboundFunctions _ = return True
