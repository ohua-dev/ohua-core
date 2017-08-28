module Ohua.DFLang.TailRec where

import Ohua.Types
import Ohua.ALang.Lang
import Ohua.Monad
import Ohua.DFLang.Lang
import Ohua.DFLang.Util
import Ohua.DFLang.Refs

import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import           Data.Foldable
import           Data.Sequence        (Seq, (<|), (|>))
import qualified Data.Sequence        as S
import qualified Data.HashSet         as HS
import qualified Data.HashMap.Strict  as HM


-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur conditionOutput algoInArgs recurArgs).
-- it is not possible to perform this kind of transformation using the HOF type class.

recursionLowering :: (MonadOhua m, MonadError String m) => Binding -> Seq LetExpr -> m (Seq LetExpr)
recursionLowering binding = transformRecursiveTailCall

transformRecursiveTailCall :: (MonadOhua m, MonadError String m) => Seq LetExpr -> m (Seq LetExpr)
transformRecursiveTailCall exprs = handleRecursiveTailCall exprs $ findExpr (DFFunction "ohua.lang/recur") exprs

handleRecursiveTailCall :: (MonadOhua m, MonadError String m) =>  Seq LetExpr -> Maybe LetExpr -> m (Seq LetExpr)
handleRecursiveTailCall dfExprs Nothing = return dfExprs
handleRecursiveTailCall dfExprs (Just recurFn) = do
        -- get the condition result
    let unAssignment x = case x of
                              Direct b -> b
                              _ -> error "invariant broken: recur always gives direct output"
    let usages = findUsages (unAssignment (returnAssignment recurFn)) dfExprs
    let switchFn = find ((== DFFunction "ohua.lang/switch") . functionRef) usages
--    let conditionOutput :: DFVar
    let conditionOutput = case switchFn of
                               Just (LetExpr _ _ _ (c:_) _) -> c
                               Nothing -> error "invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch."

        -- get the algo-in vars. note that for the recursive case this includes all free vars (even those accessed via the lexical scope).
    let algoInVars = HS.toList $ findFreeVars dfExprs
    algoInToRecurInVars <- foldM (\x y -> do s <- generateBindingWith y; return $ HM.insert y s x;) HM.empty algoInVars
    let updatedRecurExpr = renameWith algoInToRecurInVars dfExprs
    let unwrap x = case x of { Just v -> v;  Nothing -> error "invariant broken";}
    let recurInVars = map (unwrap . flip HM.lookup algoInToRecurInVars) algoInVars
    algoInVarsArrayId <- generateId
    algoInVarsArrayRet <- generateBindingWith "algo-in"
    recurInVarsArrayId <- generateId
    recurInVarsArrayRet <- generateBindingWith "recur-in"
    let algoInVarsArray = LetExpr algoInVarsArrayId (Direct algoInVarsArrayRet) (EmbedSf "ohua.lang/array") (map DFVar algoInVars) Nothing
    let recurInVarsArray = LetExpr recurInVarsArrayId (Direct recurInVarsArrayRet) (EmbedSf "ohua.lang/array") (map DFVar recurInVars) Nothing
    let updatedRecurExpr = LetExpr (callSiteId recurFn) (Destructure recurInVars) (functionRef recurFn) [conditionOutput, DFVar algoInVarsArrayRet, DFVar recurInVarsArrayRet] Nothing
    return $ algoInVarsArray <| (S.filter (/= recurFn) dfExprs |> recurInVarsArray |> updatedRecurExpr)

