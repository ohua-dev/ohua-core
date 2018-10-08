module Ohua.DFLang.TailRec where

import Ohua.Prelude

import Control.Monad.Writer (MonadWriter, tell)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S

import Ohua.ALang.Lang
import Ohua.ALang.Refs as ARefs
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util

data Recur = Recur
    { recursion :: Lambda
      initialArgs :: DFVar -- packaged in a []
    } deriving (Show)

instance HigherOrderFunction RecursiveLambda where
  hofName = tagFnName ARefs.recur

  parseCallAndInitState [LamArg lam, Variable v] = return $ Recur lam v
  parseCallAndInitState a = throwError $ "Unexpected number/type of arguments to recur" <> show a

  -- we need the outgoing arc of the lambda for the recur op
  createContextEntry = return []

  createContextExit recurResultVar = do
    (Recur (Lam lamIn lamOut) initArgs) <- get
    recurId <- generateId
    return [
            LetExpr recurId
                    (Destructure recurResultVar lamOutput)
                    Refs.recur
                    [initArgs, DFVar lamIn]
           ]

  scopeFreeVariables bindings freeVars = return ([],[]) -- TODO
  contextifyUnboundFunctions lambda = return Nothing -- TODO
