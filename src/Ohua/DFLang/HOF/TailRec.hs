module Ohua.DFLang.HOF.TailRec where

import Ohua.Prelude

-- import Control.Monad.Writer (MonadWriter, tell)
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashSet as HS
-- import Data.Sequence (Seq, (<|), (|>))
-- import qualified Data.Sequence as S

import Ohua.ALang.Refs as ARefs
import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.HOF

data Recur = Recur
    { recursion :: Lambda,
      initialArgs :: DFVar -- packaged in a []
    } deriving (Show)

instance HigherOrderFunction Recur where
  hofName = tagFnName ARefs.recur

  parseCallAndInitState [LamArg lam, Variable v] = return $ Recur lam v
  parseCallAndInitState a = throwError $ "Unexpected number/type of arguments to recur" <> show a

  -- we need the outgoing arc of the lambda for the recur op
  createContextEntry = return []

  createContextExit recurResultVar = do
    (Recur (Lam (Direct lamIn) lamOut) initArgs) <- get
    recurId <- generateId
    return [
            LetExpr recurId
                    (Destructure $ (extractBindings recurResultVar) ++ [lamOut])
                    Refs.recur
                    [initArgs, DFVar lamIn]
                    Nothing
           ]

  scopeFreeVariables bindings freeVars = return ([],[]) -- TODO

  contextifyUnboundFunctions lambda = return Nothing -- TODO
