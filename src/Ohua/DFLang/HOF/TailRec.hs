module Ohua.DFLang.HOF.TailRec where

import Ohua.Prelude

-- import Control.Monad.Writer (MonadWriter, tell)
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashSet as HS
-- import Data.Sequence (Seq, (<|), (|>))
-- import qualified Data.Sequence as S

import Ohua.ALang.Refs as ARefs
import Ohua.ALang.Passes.TailRec as TR
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.HOF

data TailRecursion = TailRecursion
    { recursion :: Lambda,
      initialArgs :: DFVar -- packaged in a []
    } deriving (Show)

instance HigherOrderFunction TailRecursion where
  hofName = tagFnName TR.recur

  parseCallAndInitState [LamArg lam, Variable v] = return $ TailRecursion lam v
  parseCallAndInitState a = throwError $ "Unexpected number/type of arguments to recur" <> show a

  -- we need the outgoing arc of the lambda for the recur op
  createContextEntry = return []

  createContextExit recurResultVar = do
    (TailRecursion (Lam (Direct lamIn) lamOut) initArgs) <- get
    recurId <- generateId
    return [
            LetExpr recurId
                    (Destructure $ (extractBindings recurResultVar) ++ [lamIn])
                    Refs.recur
                    [initArgs, DFVar lamOut]
                    Nothing
           ]

  scopeFreeVariables bindings freeVars = return ([],[]) -- TODO

  contextifyUnboundFunctions lambda = return Nothing -- TODO
