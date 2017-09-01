{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.DFLang.TailRec where

import Ohua.Types
import Ohua.ALang.Lang
import Ohua.Monad
import Ohua.DFLang.Lang
import Ohua.DFLang.Util
import qualified Ohua.DFLang.Refs     as Refs

import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import           Data.Foldable
import           Data.Sequence        (Seq, (<|), (|>))
import qualified Data.Sequence        as S
import qualified Data.HashSet         as HS
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import Debug.Trace


-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur conditionOutput algoInArgs recurArgs).

recursionLowering :: (MonadOhua m, MonadError String m) => DFExpr -> m DFExpr
--recursionLowering binding = (transformRecursiveTailCall
recursionLowering dfExpr = do
    result <- transformRecursiveTailCall $ trace ("Lambda expression:\n" ++ show dfExpr) dfExpr
    return $ trace ("-----\ngenerated tailrec transformation:\n" ++ show result) result

transformRecursiveTailCall :: (MonadOhua m, MonadError String m) => DFExpr -> m DFExpr
transformRecursiveTailCall exprs = handleRecursiveTailCall exprs $ traceShowId $ findExpr Refs.recur $ letExprs exprs

handleRecursiveTailCall :: (MonadOhua m, MonadError String m) =>  DFExpr -> Maybe LetExpr -> m DFExpr
handleRecursiveTailCall dfExpr Nothing = throwError $ "could not find recur in DFExpr:\n" ++ show dfExpr
handleRecursiveTailCall dfExpr (Just recurFn) = do
    let dfExprs = letExprs dfExpr
    -- helpers
    let unAssignment e x = case x of
                              Direct b -> b
                              _ -> error $ "invariant broken: '" ++ e ++ "' always gives direct output"
    let unMaybe id = fromMaybe (error $ "Invariant broken: should have found something but got Nothing! " ++ id)
    let unDFVar x = case x of { DFVar b -> b; _ -> error $ "invariant broken: should be DFVar but was: " ++ show x; }

    -- get the condition result
    let usages = findUsages (unAssignment "recur" (returnAssignment recurFn)) dfExprs
    let switchFn = unMaybe "switch does not use recur output" $ find ((== Refs.switch) . functionRef) usages
    -- let conditionOutput :: DFVar
    let conditionOutput = case switchFn of
                               (LetExpr _ _ _ (c:_) _) -> c
                               _ -> error "invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch."

    -- the switch is not needed anymore because the only output comes from the termination branch
    let minusSwitchExprs = S.filter (/= switchFn) dfExprs
    let switchRet = returnAssignment switchFn

    let d = HS.difference $ HS.fromList $ callArguments switchFn
    let terminationBranchOut = d $ HS.fromList [conditionOutput, DFVar $ unAssignment "recur" $ returnAssignment recurFn]
    let terminationBranch = head $ HS.toList $ assert  (1 == HS.size terminationBranchOut) terminationBranchOut

    let switchRetBnd = unAssignment "switch" switchRet
    let rewiredTOutExps = flip renameWith minusSwitchExprs $ HM.singleton switchRetBnd (unDFVar terminationBranch)
    let newLambdaRetVar = unDFVar $ flip assert terminationBranch $ returnVar dfExpr == switchRetBnd

    -- get the algo-in vars. note that for the recursive case this includes all free vars (even those accessed via the lexical scope).
    let algoInVars = HS.toList $ findFreeVars rewiredTOutExps
    algoInToRecurInVars <- foldM (\x y -> do s <- generateBindingWith y; return $ HM.insert y s x;) HM.empty algoInVars
    let updatedRecurExpr = renameWith algoInToRecurInVars rewiredTOutExps

    let recurInVars = map (unMaybe "input into recur not found" . flip HM.lookup algoInToRecurInVars) algoInVars
    algoInVarsArrayId <- generateId
    algoInVarsArrayRet <- generateBindingWith "algo-in"
    recurInVarsArrayId <- generateId
    recurInVarsArrayRet <- generateBindingWith "recur-in"
    let algoInVarsArray = LetExpr algoInVarsArrayId (Direct algoInVarsArrayRet) Refs.array (map DFVar algoInVars) Nothing
    let recurInVarsArray = LetExpr recurInVarsArrayId (Direct recurInVarsArrayRet) Refs.array (map DFVar recurInVars) $ contextArg recurFn
    let updatedRecurExpr = LetExpr (callSiteId recurFn) (Destructure recurInVars) (functionRef recurFn) [conditionOutput, DFVar algoInVarsArrayRet, DFVar recurInVarsArrayRet] Nothing
    return $ flip DFExpr
              newLambdaRetVar
              $ algoInVarsArray <| (S.filter (/= recurFn) rewiredTOutExps |> recurInVarsArray |> updatedRecurExpr)


