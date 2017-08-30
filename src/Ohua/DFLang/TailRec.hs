{-# LANGUAGE ScopedTypeVariables #-}
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

recursionLowering :: (MonadOhua m, MonadError String m) => Binding -> Seq LetExpr -> m (Seq LetExpr)
recursionLowering binding = transformRecursiveTailCall

transformRecursiveTailCall :: (MonadOhua m, MonadError String m) => Seq LetExpr -> m (Seq LetExpr)
transformRecursiveTailCall exprs = handleRecursiveTailCall exprs $ findExpr (DFFunction "ohua.lang/recur") exprs

handleRecursiveTailCall :: (MonadOhua m, MonadError String m) =>  Seq LetExpr -> Maybe LetExpr -> m (Seq LetExpr)
handleRecursiveTailCall dfExprs Nothing = return dfExprs
handleRecursiveTailCall dfExprs (Just recurFn) = do
    -- helpers
    let unAssignment e x = case x of
                              Direct b -> b
                              _ -> error $ "invariant broken: '" ++ e ++ "' always gives direct output"
    let unMaybe x = case x of { Just v -> v;  Nothing -> error "invariant broken"; }
    let unDFVar x = case x of { DFVar b -> b; _ -> error "invariant broken"; }

    -- get the condition result
    let usages = findUsages (unAssignment "recur" (returnAssignment recurFn)) dfExprs
    let switchFn = find ((== DFFunction "ohua.lang/switch") . functionRef) usages
    -- let conditionOutput :: DFVar
    let conditionOutput = case switchFn of
                               Just (LetExpr _ _ _ (c:_) _) -> c
                               Nothing -> error "invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch."

    -- the switch is not needed anymore because the only output comes from the termination branch
    let minusSwitchExprs = S.filter (/= unMaybe switchFn) dfExprs
    let switchRet = returnAssignment .unMaybe $ switchFn

    let d = HS.difference $ HS.fromList [conditionOutput, DFVar $ unAssignment "recur" $ returnAssignment recurFn]
    let terminationBranchOut = d $ HS.fromList $ callArguments $ unMaybe switchFn
    let terminationBranch = head $ HS.toList $ assert  (1 == HS.size terminationBranchOut) terminationBranchOut

    let rewiredTOutExps = flip renameWith minusSwitchExprs $ HM.singleton (unAssignment "switch" switchRet) (unDFVar terminationBranch)

    -- get the algo-in vars. note that for the recursive case this includes all free vars (even those accessed via the lexical scope).
    let algoInVars = HS.toList $ findFreeVars rewiredTOutExps
    algoInToRecurInVars <- foldM (\x y -> do s <- generateBindingWith y; return $ HM.insert y s x;) HM.empty algoInVars
    let updatedRecurExpr = renameWith algoInToRecurInVars rewiredTOutExps

    let recurInVars = map (unMaybe . flip HM.lookup algoInToRecurInVars) algoInVars
    algoInVarsArrayId <- generateId
    algoInVarsArrayRet <- generateBindingWith "algo-in"
    recurInVarsArrayId <- generateId
    recurInVarsArrayRet <- generateBindingWith "recur-in"
    let algoInVarsArray = LetExpr algoInVarsArrayId (Direct algoInVarsArrayRet) (EmbedSf "ohua.lang/array") (map DFVar algoInVars) Nothing
    let recurInVarsArray = LetExpr recurInVarsArrayId (Direct recurInVarsArrayRet) (EmbedSf "ohua.lang/array") (map DFVar recurInVars) $ contextArg recurFn
    let updatedRecurExpr = LetExpr (callSiteId recurFn) (Destructure recurInVars) (functionRef recurFn) [conditionOutput, DFVar algoInVarsArrayRet, DFVar recurInVarsArrayRet] Nothing
    return $ algoInVarsArray <| (S.filter (/= recurFn) rewiredTOutExps |> recurInVarsArray |> updatedRecurExpr)

