{-# LANGUAGE CPP #-}

module Ohua.ALang.Passes.TailRec where

import Ohua.Prelude

import Control.Monad.Writer
import Data.Functor.Foldable
import qualified Data.HashSet as HS

import Ohua.ALang.Lang
import Ohua.Unit

-- Design:
-- ============
--
-- Phase 1: (Performed directly on the initial ALang form.)
-- Find recursions and mark them, i.e., perform the following transformation:
--
-- let f = \x1 ... xn -> ...
--                      let y1 = ....
--                      ...
--                      let yn = ...
--                      ...
--                      if c then
--                          result
--                      else
--                          f y1 ... yn
--
-- into this:
--
-- letrec f = \x1 ... xn -> ...
--                          let y1 = ....
--                          ...
--                          let yn = ...
--                          ...
--                          if c then
--                              result
--                          else
--                              recur y1 ... yn
--
-- This turns the recursive function into a non-recursive one. But it would still
-- not be possible to let the other transformations run. Especially, the lambda-inlining
-- transformation would actually lose the `letrec` marker.
--
--
-- Phase 2: (Performed on the output of Phase 1.)
-- Turn recursions into HOFs:
--
-- letrec f' = \x1 ... xn -> ...
--                          let y1 = ....
--                          ...
--                          let yn = ...
--                          ...
--                          if c then
--                              result
--                          else
--                              recur y1 ... yn
-- let f = recur_hof f'
--
-- Lambda-inlinling will then just inline f' while still performing all other transformations
-- on it. A nice benefit: lowering for tail recursion is just an implementation of
-- a HigherOrderFunction lowering. As such though, we can not access the lambda, i.e., f.
-- So we need to do the lambda modifications on ALang before (which is nicer anyways).
--
-- Phase 3: (Performed on the expression in ALang-normalized form (ANF)!)
-- Rewrite the code (for a call) such that:
--
-- let g = recur (\x -> let (x1 ... xn) = x
--                             ...
--                             let y1 = ....
--                             ...
--                             let yn = ...
--                             ...
--                             let e = if c then
--                                       let z = right result
--                                       in z
--                                     else
--                                       let ys = array y1 ... yn
--                                       let z = left ys
--                                       in z
--                             in e
--               )
--               array a1 ... an
--
-- Note: recur_hof became recur
-- Either might be also implemented as:
-- right a = (false, a)
-- left a = (true, a)
-- isRight = not . fst
-- isLeft = fst
--
-- Phase 4: (DF Lowering for recur_hof.)
-- add a the following operator as a context op:
--
-- recur [a1 ... an] Either [y1 ... yn] result -> Either [y1 ... yn] result
--
-- The operator has two incoming arcs and two outgoing arcs:
-- 1. incoming arc: [a1 ... an]
-- 2. incoming arc: Either [y1 ... yn] result  <-- feedback edge: e
-- 1. outgoing arc: Either [a1 ... an] [y1 ... yn]
-- 2. outgoing arc: result

--  ==== Implementation starts here

-- Phase 1:
findTailRecs :: Expression -> Expression
findTailRecs = snd . (flip findRecCall HS.empty)

findRecCall :: Expression -> HS.HashSet Binding -> (HS.HashSet Binding, Expression)
findRecCall (Let (Direct a) expr inExpr) algosInScope
    -- did I detect a reference to this binding in the assignment expr?
    | HS.member a found = (iFound, Let (Recursive a) e iExpr)
    | otherwise = (HS.union found iFound, Let (Direct a) e iExpr)
  where
    -- for the assigment expr I add the reference and check the expression for references to the identifier
    (found, e) = findRecCall expr $ HS.insert a algosInScope
    -- proceed normally into the next expression
    (iFound, iExpr) = findRecCall inExpr algosInScope

findRecCall (Let a expr inExpr) algosInScope =
    let (iFound, iExpr) = findRecCall inExpr algosInScope
     in (iFound, Let a expr iExpr)
findRecCall (Apply (Var (Local binding)) a) algosInScope
    | HS.member binding algosInScope
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
     = (HS.insert binding HS.empty, Apply "ohua.lang/recur" a)
findRecCall (Apply a b) algosInScope =
    let (aFound, aExpr) = findRecCall a algosInScope
        (bFound, bExpr) = findRecCall b algosInScope
     in (HS.union aFound bFound, Apply aExpr bExpr)
findRecCall (Var b) _ = (HS.empty, Var b)
findRecCall (Lambda a e) algosInScope =
    let (eFound, eExpr) = findRecCall e algosInScope
     in if HS.size eFound == 0
            then (eFound, Lambda a eExpr)
            else (eFound, Lambda a eExpr)

-- Phase 2:
hoferize :: (Monad m, MonadGenBnd m) => Expression -> m Expression
hoferize (Let (Recursive f) expr inExpr) = do
  f' <- generateBindingWith f
  return $ Let (Direct f') expr
              $ Let (Direct f) (Apply (Var (Sf "ohua.lang/recur_hof" Nothing)) (Var (Local f')))
                    inExpr
hoferize (Let v expr inExpr) = Let v <$> hoferize expr <*> hoferize inExpr
hoferize (Apply a b) = Apply <$> hoferize a <*> hoferize b
hoferize (Lambda a e) = Lambda a <$> hoferize e
hoferize v@(Var _) = return v

-- Phase 3:
rewrite :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
rewrite e | isRecurHofCall e = rewriteCallExpr e
rewrite (Let v expr inExpr) = Let v <$> rewrite expr <*> rewrite inExpr
rewrite (Apply a b) = Apply <$> rewrite a <*> rewrite b
rewrite (Lambda a e) = Lambda a <$> rewrite e
rewrite v@(Var _) = return v

isRecurHofCall (Apply (Var (Sf "ohua.lang/recur_hof" _)) _) = True
isRecurHofCall (Apply e@(Apply _ _) _) = isRecurHofCall e
isRecurHofCall _ = False

rewriteCallExpr :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
rewriteCallExpr e = do
  let ( (Var (Sf "ohua.lang/recur_hof" _)) : (lamExpr : initialArgs) ) = fromApplyToList e
  let arrayfiedArgs = fromListToApply (Sf "ohua.lang/array" Nothing) initialArgs
  lamExpr' <- rewriteLambdaExpr lamExpr
  return $ Apply (Apply (Var (Sf "ohua.lang/recur" Nothing)) lamExpr') arrayfiedArgs

rewriteLambdaExpr :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
rewriteLambdaExpr (Lambda vars expr) = do
  x <- generateBinding
  expr' <- rewriteLastCond expr
  return $ Lambda (Direct x) $ Let (Destructure (extractBindings vars)) (Var (Local x)) expr'
  where
    rewriteLastCond :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
    rewriteLastCond (Let v e o@(Var _)) = (\e' -> Let v e' o) <$> rewriteCond e
    rewriteLastCond (Let v e ie) = Let v e <$> rewriteLastCond ie

    rewriteCond :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
    rewriteCond (Apply (Apply c@(Apply (Var (Sf "ohua.lang/if" Nothing)) cond)
                                       (Lambda a trueB))
                                       (Lambda b falseB)) = do
      trueB'  <- rewriteBranch trueB
      falseB' <- rewriteBranch falseB
      return $ Apply (Apply c (Lambda a trueB')) (Lambda b falseB')
    rewriteCond _ = error "invariant broken: recursive function does not have the proper structure."

    rewriteBranch :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
    rewriteBranch (Let v e@(Apply (Var (Sf "ohua.lang/id" Nothing)) _) o) = return $ Let v (rewriteTerminationBranch e) o
    rewriteBranch (Let v e o) = (\e' -> Let v e' o) <$> (rewriteRecursionBranch e $ fromApplyToList e)

    rewriteTerminationBranch e =
      (Apply (Apply (Var (Sf "ohua.lang/mkTuple" Nothing))
                    (Apply (Var (Sf "ohua.lang/false" Nothing)) unitExpr ))
              e)

    rewriteRecursionBranch e ( (Var (Sf "ohua.lang/recur" _)) : vars ) = return
      (Apply (Apply (Var (Sf "ohua.lang/mkTuple" Nothing))
                    (Apply (Var (Sf "ohua.lang/true" Nothing)) unitExpr ))
              $ replaceFn (Sf "ohua.lang/array" Nothing) e)
                  -- fromListToApply (Sf "ohua.lang/array") $ reverse vars)
    rewriteRecursionBranch _ _ = error "invariant broken"
rewriteLambdaExpr _ = error "invariant broken"

replaceFn fn (Apply (Var (Sf _ _)) v) = Apply (Var fn) v
replaceFn fn (Apply f v) = Apply (replaceFn fn f) v

fromListToApply f (v:[]) = Apply (Var f) v
fromListToApply f (v:vs) = Apply (fromListToApply f vs) v

fromApplyToList (Apply f@(Var _) v) = [f, v]
fromApplyToList (Apply a b) = fromApplyToList a ++ [b]

--  ==== Implementation ends here

markRecursiveBindings :: Expression -> Expression
markRecursiveBindings = fst . runWriter . cata go
  where
    go (LetF assign e b)
      -- We censor here as this binding would shadow bindings from outside
     =
        shadowAssign assign $ do
            (e', isUsed) <-
                listens
                    (not .
                     null . HS.intersection (HS.fromList (extractBindings assign)))
                    e
            if isUsed
                then case assign of
                         Direct bnd -> Let (Recursive bnd) e' <$> b
                         _ -> error "Cannot use destrutured binding recursively"
                else Let assign e' <$> b
    go (VarF val@(Local bnd)) = tell (HS.singleton bnd) >> pure (Var val)
    go e@(LambdaF assign _) = shadowAssign assign $ embed <$> sequence e
    go e = embed <$> sequence e
    shadowAssign (Direct b) = censor (HS.delete b)
    shadowAssign (Destructure bnds) = censor (`HS.difference` HS.fromList bnds)
    shadowAssign (Recursive _) =
        error "TODO implement `shadowAssign` for `Recursive`"
