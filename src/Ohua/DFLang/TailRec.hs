{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Ohua.DFLang.TailRec where

import           Ohua.ALang.Lang
import           Ohua.ALang.Refs            as ALangRefs
import           Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs           as Refs
import           Ohua.DFLang.Util
import           Ohua.Monad                 as M
import           Ohua.Types                 as Ty

import           Control.Exception
import           Control.Monad.Freer.Writer

import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence              (Seq, (<|), (|>))
import qualified Data.Sequence              as S
import           Ohua.Util
import qualified Ohua.Util.Str              as Str


-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur conditionOutput algoInArgs recurArgs).

data RecursiveLambdaSpec = RecursiveLambdaSpec
    { formalInputs          :: [Binding] -- as defined by the expression
    , lambdaFormalsToAlgoIn :: HM.HashMap Binding Binding
    , dfExpr                :: DFExpr
    } deriving Show

-- | Executed to generate the recursive lambda expression.
recursionLowering :: Members '[M.Error Ty.Error, GenId, GenBnd] effs => [Binding] -> DFExpr -> Eff effs RecursiveLambdaSpec
--recursionLowering binding = (transformRecursiveTailCall
recursionLowering lambdaFormals dfExpr0 = do
    result <- transformRecursiveTailCall lambdaFormals $ trace ("Lambda expression:\n" ++ show dfExpr0) dfExpr0
    return $ trace ("-----\ngenerated tailrec transformation:\n" ++ show result) result

transformRecursiveTailCall :: Members '[M.Error Ty.Error, GenId, GenBnd] effs => [Binding] -> DFExpr -> Eff effs RecursiveLambdaSpec
transformRecursiveTailCall lambdaFormals exprs = handleRecursiveTailCall lambdaFormals exprs $ traceShowId $ findExpr Refs.recur $ letExprs exprs

handleRecursiveTailCall :: Members '[M.Error Ty.Error, GenBnd, GenId] effs => [Binding] -> DFExpr -> Maybe LetExpr -> Eff effs RecursiveLambdaSpec
handleRecursiveTailCall _ dfExpr0 Nothing = failWith $ "could not find recur in DFExpr:\n" <> Str.showS dfExpr0
handleRecursiveTailCall lambdaFormals dfExpr0 (Just recurFn) = do
    let dfExprs = letExprs dfExpr0
    -- helpers
    let unAssignment e x = case x of
                              Direct b -> b
                              _        -> error $ "invariant broken: '" ++ e ++ "' always gives direct output"
    let unMaybe fid = fromMaybe (error $ "Invariant broken: should have found something but got Nothing! " ++ fid)
    let unDFVar x = case x of { DFVar b -> b; _ -> error $ "invariant broken: should be DFVar but was: " ++ show x; }

    -- get the condition result
    let usages = findUsages (unAssignment "recur" (returnAssignment recurFn)) dfExprs
    let switchFn = unMaybe "switch does not use recur output" $ find ((== Refs.switch) . functionRef) usages
    -- let conditionOutput :: DFVar
    let conditionOutput = case switchFn of
                               (LetExpr _ _ _ (c:_) _) -> c
                               _                       -> error "invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch."

    -- the switch is not needed anymore because the only output comes from the termination branch
    let minusSwitchExprs = S.filter (/= switchFn) dfExprs
    let switchRet = returnAssignment switchFn

    let d = HS.difference $ HS.fromList $ callArguments switchFn
    let terminationBranchOut = d $ HS.fromList [conditionOutput, DFVar $ unAssignment "recur" $ returnAssignment recurFn]
    let terminationBranch = head $ HS.toList $ assert  (1 == HS.size terminationBranchOut) terminationBranchOut

    let switchRetBnd = unAssignment "switch" switchRet
    let rewiredTOutExps = flip renameWith minusSwitchExprs $ HM.singleton switchRetBnd (unDFVar terminationBranch)
    let newLambdaRetVar = unDFVar $ flip assert terminationBranch $ returnVar dfExpr0 == switchRetBnd

    -- get the algo-in vars. note that for the recursive case this includes all free vars (even those accessed via the lexical scope).
    -- FIXME this should not be necessary once we implemented lambda lifting on ALang!
    let lambdaInVars = HS.toList $ findFreeVars rewiredTOutExps
    lambdaInToRecurInVars <- foldM (\x y -> do s <- generateBindingWith y; return $ HM.insert y s x;) HM.empty lambdaInVars -- TODO beware to pertain the ordering here!
    let updatedRecurExpr = renameWith lambdaInToRecurInVars rewiredTOutExps

    let recurInVars = callArguments recurFn
--    let recursionInVars = map (unMaybe "input into recur not found" . flip HM.lookup algoInToRecurInVars) $ trace ("algoInToRecurVars: " ++ show algoInToRecurInVars) algoInVars
    algoInVarsArrayId <- generateId
    algoInVarsArrayRet <- generateBindingWith "algo-in"
    recurInVarsArrayId <- generateId
    recurInVarsArrayRet <- generateBindingWith "recur-in"
    -- the args that are input to the algo-in-array function must come from the outside. hence the need to use the var that will later be bound to the actuals! (instead of the formal var)
    let algoInVarsArray = LetExpr algoInVarsArrayId (Direct algoInVarsArrayRet) Refs.array (map (DFVar . unMaybe "formal args" . flip HM.lookup lambdaInToRecurInVars) lambdaInVars) Nothing
    let recurInVarsArray = LetExpr recurInVarsArrayId (Direct recurInVarsArrayRet) Refs.array recurInVars $ contextArg recurFn
    -- remember: 'recur' produces the formals used inside the lambda expression!
    let updatedRecurExpr = LetExpr (callSiteId recurFn) (Destructure lambdaFormals) (functionRef recurFn) [conditionOutput, DFVar algoInVarsArrayRet, DFVar recurInVarsArrayRet] Nothing
    return
        $ RecursiveLambdaSpec lambdaFormals lambdaInToRecurInVars
        $ flip DFExpr (trace ("new return var: " ++ show newLambdaRetVar) newLambdaRetVar)
        $ algoInVarsArray <| (S.filter (/= recurFn) rewiredTOutExps |> recurInVarsArray |> updatedRecurExpr)


-- | Executed whenever an initial call to a recursive algorithm is detected during the lowering.
lowerRecAlgoCall :: Members '[Writer (Seq LetExpr), GenId] effs
                 => (QualifiedBinding -> FnId -> Assignment -> [Expression] -> Eff effs (Seq LetExpr))
                 -> [Expression]
                 -> RecursiveLambdaSpec
                 -> Eff effs (QualifiedBinding, FnId, [Expression])
lowerRecAlgoCall lowerDefault actuals (RecursiveLambdaSpec formalInputs lambdaFormalsToAlgoIn dfExpr) = do
        -- input side

        mapM_ (\(x, y) -> do
                id <- generateId
                (tell. traceShowId) =<< mkIdFn id x y)
            $ zip actuals
            $ map Direct
            $ trace ("input formals: " ++ show algoInInputFormals) algoInInputFormals

        -- recreate the body and 'tell' it to the writer
        tell $ letExprs dfExpr

        -- output side
        id <- generateId
        return
            $ trace ("output side: " ++ show (returnVar dfExpr))
            $ (ALangRefs.id, id, [Var $ Local $ returnVar dfExpr])
  where
    mkIdFn id i o = lowerDefault ALangRefs.id id o [i]
    algoInInputFormals =
        map (fromMaybe (error "Invariant broken")
        . flip HM.lookup lambdaFormalsToAlgoIn) formalInputs
