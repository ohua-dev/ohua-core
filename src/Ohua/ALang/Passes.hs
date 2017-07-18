-- |
-- Module      : $Header$
-- Description : Passes over algorithm language terms to ensure certain invariants
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Passes where


import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe           (fromMaybe)
import           Lens.Micro
import           Lens.Micro.Mtl
import           Ohua.ALang.Lang
import           Ohua.ALang.Util
import           Ohua.IR              hiding (Destructure, Direct)
import           Ohua.IR.Functions
import           Ohua.LensClasses
import           Ohua.Monad
import           Ohua.Types


-- instead of doing a traversal, we should transform the expression in the following steps:
-- 1. make implicit passing explicit by binding the result
-- 2. contextify
-- 3. create SSA
-- 4. perform lambda lifting to derive just a single let statement


-- -- step: turn implicit into explicit
-- mkDataflowExplicit :: Expression -> Expression
-- mkDataflowExplicit (Let l e) = return $ Let (map convertAssignment l) (mkDataflowExplicit e)

-- mkDataflowExplicit (Var v) = return $ Var v


-- mkDataflowExplicit (Lambda as e) = return $ Lambda as $ mkDataflowExplicit e

-- mkDataflowExplicit (Apply (Apply f s) e) = mkDataflowExplicit $
--                               Let
--                                [(Direct (Binding "something"), Apply f s)]
--                               $ Apply (Var (Local (Binding "something"))) e
-- mkDataflowExplicit (Apply r as) = convertApply r as []


-- convertApply :: Expression -> [Expression] -> [Expression] -> Expression
-- convertApply r (x@(Apply f as):xs) n = Let
--                                          [ (Direct (Binding "something"), mkDataflowExplicit (Apply f as)) ]
--                                          $ convertApply r xs (n ++ [(Var (Local (Binding "something")))])
-- convertApply r (x:xs) n = convertApply r xs $ n ++ [(mkDataflowExplicit x)]
-- convertApply r [] n = Apply r n

-- convertAssignment :: (Assignment, Expression) -> (Assignment, Expression)
-- convertAssignment t@(a,e) = (a, (mkDataflowExplicit e))


-- contexts: seq, if, higher-order functions (any function that takes an algo as an input)
contextify :: Expression -> Expression
contextify e = undefined


type Error = String

inlineLambdaRefs :: MonadError Error m => Expression -> m Expression
inlineLambdaRefs (Let (Direct bnd) l@(Lambda _ _) body) = inlineLambdaRefs $ substitute bnd l body
inlineLambdaRefs (Let (Destructure _) (Lambda _ _) _) = throwError "invariant broken, cannot destructure lambda"
inlineLambdaRefs (Let assignment value body) = Let assignment <$> inlineLambdaRefs value <*> inlineLambdaRefs body
inlineLambdaRefs (Apply body argument) = liftM2 Apply (inlineLambdaRefs body) (inlineLambdaRefs argument)
inlineLambdaRefs (Lambda argument body) = Lambda argument <$> inlineLambdaRefs body
inlineLambdaRefs e = return e


-- Assumes lambda refs have been inlined
inlineLambda :: Expression -> Expression
inlineLambda e@(Var _) = e
inlineLambda (Apply (Lambda assignment body) argument) = Let assignment argument $ inlineLambda body
inlineLambda (Apply v@(Var _) argument) = Apply v (inlineLambda argument)
inlineLambda (Apply v@(Apply _ _) argument) = reduceLetCWith f (inlineLambda v)
  where
    f (Lambda assignment body) = Let assignment inlinedArg body
    f v = Apply v inlinedArg
    inlinedArg = inlineLambda argument
inlineLambda v@(Apply _ _) = reduceLetCWith inlineLambda v
inlineLambda (Let name value body) = Let name (inlineLambda value) (inlineLambda body)
inlineLambda (Lambda param body) = Lambda param $ inlineLambda body

-- recursively performs the substitution
--
-- let x = (let y = M in A) in E[x] -> let y = M in let x = A in E[x]
reduceLetA :: Expression -> Expression
reduceLetA (Let assign (Let assign2 val expr3) expr) = Let assign2 val $ reduceLetA $ Let assign expr3 expr
reduceLetA e = e

reduceLetCWith :: (Expression -> Expression) -> Expression -> Expression
reduceLetCWith f (Apply (Let assign val expr) argument) = Let assign val $ reduceLetCWith f $ Apply expr argument
reduceLetCWith f e = f e

reduceLetC :: Expression -> Expression
reduceLetC = reduceLetCWith id

reduceAppArgument :: Expression -> Expression
reduceAppArgument (Apply function (Let assign val expr)) = Let assign val $ reduceApplication $ Apply function expr
reduceAppArgument e = e

-- recursively performs the substitution
--
-- (let x = M in A) N -> let x = M in A N
--
-- and then
--
-- A (let x = M in N) -> let x = M in A N
reduceApplication :: Expression -> Expression
reduceApplication = reduceLetCWith reduceAppArgument


letLift :: Expression -> Expression
letLift (Let assign expr expr2) = reduceLetA $ Let assign (letLift expr) (letLift expr2)
letLift (Apply function argument) = reduceApplication $ Apply (letLift function) (letLift argument)
letLift (Lambda bnd body) = Lambda bnd (letLift body)
letLift v = v


inlineReassignments :: Expression -> Expression
inlineReassignments (Let assign val@(Var v) body) =
    case assign of
        Direct bnd2 -> inlineReassignments $ substitute bnd2 val body
        Destructure bnds -> Let assign (Apply (Var (Sf idName Nothing)) val) $ inlineReassignments body
inlineReassignments (Let assign val body) = Let assign val $ inlineReassignments body
inlineReassignments (Apply e1 e2) = Apply (inlineReassignments e1) (inlineReassignments e2)
inlineReassignments (Lambda assign e) = Lambda assign $ inlineReassignments e
inlineReassignments v@(Var _) = v


ensureFinalLet :: MonadOhua m => Expression -> m Expression
ensureFinalLet (Let a e b) = Let a e <$> ensureFinalLet b
ensureFinalLet v@(Var _) = return v
ensureFinalLet a = do
    newBnd <- generateBinding
    return $ Let (Direct newBnd) a (Var (Local newBnd))


-- assumes ssa for simplicity
removeUnusedBindings :: Expression -> Expression
removeUnusedBindings = fst . go
  where
    go :: Expression -> (Expression, HS.HashSet Binding)
    go v@(Var (Local b)) = (v, HS.singleton b)
    go v@(Var _) = (v, mempty)
    go (Lambda bnd body) = first (Lambda bnd) $ go body
    go (Apply e1 e2) = (Apply e1' e2', u1 `HS.union` u2)
      where
        (e1', u1) = go e1
        (e2', u2) = go e2
    go (Let bnds val body) = (newExpr, used)
      where
        (inner, bodyUsed) = go body
        (valNew, valUsed) = go val
        used = valUsed `HS.union` bodyUsed
        newExpr
            | not $ any (`HS.member` used) $ flattenAssign bnds = inner
            | otherwise = Let bnds valNew inner



hasFinalLet :: MonadError String m => Expression -> m ()
hasFinalLet (Let _ _ body) = hasFinalLet body
hasFinalLet (Var (Local _))        = return ()
hasFinalLet (Var _) = throwError "Non-local final var"
hasFinalLet _              = throwError "Final value is not a var"


lamdasAreInputToHigherOrderFunctions :: MonadError Error m => Expression -> m ()
lamdasAreInputToHigherOrderFunctions _ = return ()
lamdasAreInputToHigherOrderFunctions (Apply v (Lambda _ body)) = undefined


checkProgramValidity :: MonadError Error m => Expression -> m ()
checkProgramValidity e = do
    hasFinalLet e



normalize :: (MonadOhua m, MonadError Error m) => Expression -> m Expression
normalize = reduceLambdas . letLift >=> ensureFinalLet . inlineReassignments
  where
    -- we repeat this step until a fix point is reached.
    -- this is necessary as lambdas may be input to lambdas,
    -- which means after inlining them we may ba able again to
    -- inline a ref and then inline the lambda.
    -- I doubt this will ever do more than two or three iterations,
    -- but to make sure it accepts every valid program this is necessary.
    reduceLambdas expr = do
        res <- letLift . inlineLambda <$> inlineLambdaRefs expr
        if res == expr
            then return res
            else reduceLambdas res


-- letLift (Let assign1 (Let assign2 expr2 expr3) expr4) = letLift $ Let assign2 expr2 $ Let assign1 expr3 expr4
-- letLift (Let assign v@(Var _) expr) = Let assign v $ letLift expr
-- letLift (Let assign val expr) =
--     case letLift val of
--         v'@(Let _ _ _) -> letLift $ Let assign v' expr
--         _ -> Let assign v' $ letLift expr
-- letLift e@(Var _) = e
-- letLift (Apply v@(Var _) argument) = Apply v (letLift argument)
-- letLift (Apply (Let assign expr function) argument) = letLift $ Let assign expr $ Apply function argument
-- letLift (Apply function argument) =
--     case letLift argument of
--         v'@()
