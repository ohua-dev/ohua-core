{-|
Module      : $Header$
Description : Implementation for basic tail recrusion support.
Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:

The tail recursion implementation for the ohua-core compiler encompasses the following phases:

=== Phase 1: (Performed directly on the initial ALang form.)
Find recursions and mark them, i.e., perform the following transformation:

@
  let f = \x1 ... xn -> ...
                       let y1 = ....
                       ...
                       let yn = ...
                       ...
                       if c then
                           result
                       else
                           f y1 ... yn
@

into this:

@
  letrec f = \x1 ... xn -> ...
                           let y1 = ....
                           ...
                           let yn = ...
                           ...
                           if c then
                               result
                           else
                               recur y1 ... yn
@

This turns the recursive function into a non-recursive one. But it would still
not be possible to let the other transformations run. Especially, the lambda-inlining
transformation would actually lose the `letrec` marker.


=== Phase 2: (Performed on the output of Phase 1.)
Turn recursions into HOFs:

@
  let f' = \x1 ... xn -> ...
                           let y1 = ....
                           ...
                           let yn = ...
                           ...
                           if c then
                               result
                           else
                               recur y1 ... yn
  let f = recur_hof f'
@

Lambda-inlinling will then just inline f' while still performing all other transformations
on it. A nice benefit: lowering for tail recursion is just an implementation of
a HigherOrderFunction lowering. As such though, we can not access the lambda, i.e., f.
So we need to do the lambda modifications on ALang before (which is nicer anyways).

=== Phase 3: (Performed on the expression in ALang-normalized form (ANF)!)
Rewrite the code (for a call) such that:

@
  let g = recur (\args -> let (x1 ... xn) = args
                              ...
                              let y1 = ....
                              ...
                              let yn = ...
                              ...
                              let e = if c then
                                        let z = right result
                                        in z
                                      else
                                        let ys = array y1 ... yn
                                        let z = left ys
                                        in z
                              in e
                )
                array a1 ... an
@

Note: recur_hof became recur
Either might be also implemented as:

@
  right a = (false, a)
  left a = (true, a)
  isRight = not . fst
  isLeft = fst
@

Phase 4: Handling free variables in the Lambda expression

The ALang phase handles free variables via lambda lifting:

@
  let g = recur (\b ->
                    let (args, freeArgs) = b
                    let (x1 ... xn) = args
                    let (a1 ... am) = freeArgs
                              ...
                              let y1 = ....
                              ...
                              let yn = ...
                              ...
                              let e = if c then
                                        let z = right result
                                        in z
                                      else
                                        let ys = array y1 ... yn
                                        let z = left ys
                                        in z
                              in e
                )
                array a1 ... an
                array arg1 ... argm
@

We again pack them in an array to make the recur operator easier to implement for a backend.
This way, it does not need to support varariable argument lists.

=== Phase 5: (DF Lowering for recur_hof.)
Adds the following operator as a context op:

@
  recur [a1 ... an] Either [y1 ... yn] result -> Either [y1 ... yn] result
@

The operator has two incoming arcs and two outgoing arcs:

  [1. incoming arc] @[a1 ... an]@
  [2. incoming arc] @Either [y1 ... yn] result@  <-- feedback edge: @e@
  [1. outgoing arc] @Either [a1 ... an] [y1 ... yn]@
  [2. outgoing arc] @result@

-}
{-# LANGUAGE CPP #-}

module Ohua.ALang.Passes.TailRec where

import Ohua.Prelude

import Control.Monad.Writer
import Data.Functor.Foldable
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Ohua.ALang.Lang
import Ohua.ALang.Passes (lambdaLifting)
import Ohua.ALang.Refs as ALangRefs
import Ohua.Unit
import Ohua.Configuration
import Ohua.ALang.PPrint (quickRender)


--  ==== Implementation starts here

loadTailRecPasses :: Bool -> CustomPasses env -> CustomPasses env
loadTailRecPasses False passes@(CustomPasses{ passBeforeNormalize = bn }) = passes { passBeforeNormalize = (bn . findTailRecs False) }
loadTailRecPasses True passes@(CustomPasses{ passBeforeNormalize = bn,
                                             passAfterNormalize = an }) = passes { passBeforeNormalize = (\e -> bn =<< (hoferize $ findTailRecs True e)),
                                                                                   passAfterNormalize  = (\e -> an =<< rewrite e) }

-- Currently not exposed by the frontend but only as the only part of recursion
-- at the backend.
recur :: QualifiedBinding
recur = "ohua.lang/recur"

-- This is a compiler-internal higher-order function.
recur_hof :: QualifiedBinding
recur_hof = "ohua.lang/recur_hof"

recur_sf :: Expression
recur_sf = Var $ Sf recur Nothing

recur_hof_sf :: Expression
recur_hof_sf = Var $ Sf recur_hof Nothing

-- Phase 1:
findTailRecs :: Bool -> Expression -> Expression
findTailRecs enabled = snd . flip runReader enabled . flip findRecCall HS.empty

findRecCall :: Expression -> HS.HashSet Binding -> Reader Bool (HS.HashSet Binding, Expression)
findRecCall (Let (Direct a) expr inExpr) algosInScope = do
    -- for the assigment expr I add the reference and check the expression for references to the identifier
    (found, e) <- findRecCall expr $ HS.insert a algosInScope
    -- proceed normally into the next expression
    (iFound, iExpr) <- findRecCall inExpr algosInScope
    -- did I detect a reference to this binding in the assignment expr?
    return $ if HS.member a found
             then (iFound, Let (Recursive a) e iExpr)
             else (HS.union found iFound, Let (Direct a) e iExpr)

findRecCall (Let a expr inExpr) algosInScope = do
    (iFound, iExpr) <- findRecCall inExpr algosInScope
    return (iFound, Let a expr iExpr)
findRecCall (Apply (Var (Local binding)) a) algosInScope
    | HS.member binding algosInScope
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
     = do
       enabledTR <- ask
       if enabledTR
       then return (HS.insert binding HS.empty, Apply recur_sf a)
       else error $ "Detected recursion although tail recursion support is not enabled!"
       -- else error $ "Detected recursion (" ++ (show binding) ++ ") although tail recursion support is not enabled!"
findRecCall (Apply a b) algosInScope = do
    (aFound, aExpr) <- findRecCall a algosInScope
    (bFound, bExpr) <- findRecCall b algosInScope
    return (HS.union aFound bFound, Apply aExpr bExpr)
findRecCall (Var b) _ = return (HS.empty, Var b)
findRecCall (Lambda a e) algosInScope = do
    (eFound, eExpr) <- findRecCall e algosInScope
    return $ if HS.size eFound == 0
             then (eFound, Lambda a eExpr)
             else (eFound, Lambda a eExpr)

-- Phase 2:
hoferize :: (Monad m, MonadGenBnd m) => Expression -> m Expression
hoferize (Let (Recursive f) expr inExpr) = do
  f' <- generateBindingWith f
  return $ Let (Direct f') expr
              $ Let (Direct f) (Apply recur_hof_sf (Var (Local f')))
                    inExpr
hoferize (Let v expr inExpr) = Let v <$> hoferize expr <*> hoferize inExpr
hoferize (Apply a b) = Apply <$> hoferize a <*> hoferize b
hoferize (Lambda a e) = Lambda a <$> hoferize e
hoferize v@(Var _) = return v
hoferize e = error $ show e

-- performed after normalization
verifyTailRecursion :: (Monad m, MonadGenBnd m) => Expression -> m Expression
verifyTailRecursion e | isCall recur_hof e = (performChecks $ fromApplyToList e) >> return e
  where
    performChecks (_:((Lambda a e):_)) = traverseToLastCall checkIf e
    performChecks (_:(e:_)) = error $ T.pack $ "Recursion is not inside a lambda but: " ++ (show e)

    traverseToLastCall check (Let v e ie) | isLastStmt ie = check e
    traverseToLastCall check (Let v e ie) = failOnRecur e >> traverseToLastCall check ie
    traverseToLastCall _ e = error $ T.pack $ "Invariant broken! Found expression: " ++ (show $ quickRender e)

    -- failOnRecur (Let _ e ie) | isCall recur e || isCall recur ie = error "Recursion is not tail recursive!"
    failOnRecur (Let _ e ie) = failOnRecur e >> failOnRecur ie
    failOnRecur (Lambda v e) = failOnRecur e -- TODO maybe throw a better error message when this happens
    failOnRecur (Apply (Var (Sf recur _)) _) = error "Recursion is not tail recursive!"
    failOnRecur (Apply a b) = return ()
    failOnRecur e = error $ T.pack $ "Invariant broken! Found pattern: " ++ (show e)

    checkIf e | isCall "ohua.lang/if" e = do
      -- assumes well-structured if
      let (_:(_:(tBranch:(fBranch:_)))) = fromApplyToList e
      let (Lambda v et) = tBranch
      let (Lambda v ef) = fBranch
      let lastFnOnBranch = traverseToLastCall (return . (\(Var (Sf f _)) -> f::QualifiedBinding) . head . NE.fromList . fromApplyToList)
      tFn <- lastFnOnBranch et
      fFn <- lastFnOnBranch ef
      if tFn == recur
      then if fFn == recur
           then error "Endless loop detected: Tail recursion does not have a non-recursive branch!"
           else return ()
      else if fFn == recur
           then return ()
           else error $ T.pack $ "We currently do not support recursive calls that are located on"
                              ++ "nested conditional branches (#conditional branches > 1) or in"
                              ++ "Lambdas to other higher-order functions! Found: "
                              ++ (show fFn)
                              ++ " : "
                              ++ (show tFn)
    checkIf e = error $ T.pack $ "Recursion is not tail recursive! Last stmt: " ++ (show $ quickRender e)

    isLastStmt (Var (Local _)) = True
    isLastStmt _ = False
verifyTailRecursion e@(Let v expr inExpr) = verifyTailRecursion expr >> verifyTailRecursion inExpr >> return e
verifyTailRecursion e@(Var _) = return e
verifyTailRecursion e = error $ T.pack $ "Invariant broken! Found stmt: " ++ (show e)

-- Phase 3:
rewrite :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
rewrite e | isCall recur_hof e = rewriteCallExpr e
rewrite (Let v expr inExpr) = Let v <$> rewrite expr <*> rewrite inExpr
rewrite (Apply a b) = Apply <$> rewrite a <*> rewrite b
rewrite (Lambda a e) = Lambda a <$> rewrite e
rewrite v@(Var _) = return v

isCall f (Apply (Var (Sf f' _)) _) | f == f' = True
isCall f (Apply e@(Apply _ _) _) = isCall f e
isCall _ _ = False

rewriteCallExpr :: (MonadGenBnd m, MonadError Error m) => Expression -> m Expression
rewriteCallExpr e = do
  let ( (Var (Sf recur_hof _)) : (lamExpr : initialArgs) ) = fromApplyToList e
  let arrayfiedArgs = fromListToApply (Sf ALangRefs.array Nothing) initialArgs
  lamExpr' <- rewriteLambdaExpr lamExpr
  return $ Apply (Apply recur_sf lamExpr') arrayfiedArgs

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
      (Apply (Apply (Var (Sf ALangRefs.mkTuple Nothing))
                    (Apply (Var (Sf ALangRefs.false Nothing)) unitExpr ))
              e)

    rewriteRecursionBranch e ( (Var (Sf recur _)) : vars ) = return
      (Apply (Apply (Var (Sf ALangRefs.mkTuple Nothing))
                    (Apply (Var (Sf ALangRefs.true Nothing)) unitExpr ))
              $ replaceFn (Sf ALangRefs.array Nothing) e)
                  -- fromListToApply (Sf "ohua.lang/array") $ reverse vars)
    rewriteRecursionBranch _ _ = error "invariant broken"
rewriteLambdaExpr _ = error "invariant broken"

replaceFn fn (Apply (Var (Sf _ _)) v) = Apply (Var fn) v
replaceFn fn (Apply f v) = Apply (replaceFn fn f) v

fromListToApply f (v:[]) = Apply (Var f) v
fromListToApply f (v:vs) = Apply (fromListToApply f vs) v

fromApplyToList (Apply f@(Var _) v) = [f, v]
fromApplyToList (Apply a b) = fromApplyToList a ++ [b]

-- | Phase 4: Free variable handling via lambda lifting
freeVarHandling :: (Monad m, MonadGenBnd m) => Expression -> m Expression
freeVarHandling e | isCall recur e = liftLambda e
freeVarHandling (Let v expr inExpr) = Let v <$> freeVarHandling expr <*> freeVarHandling inExpr
freeVarHandling (Apply a b) = Apply <$> freeVarHandling a <*> freeVarHandling b
freeVarHandling (Lambda a e) = Lambda a <$> freeVarHandling e
freeVarHandling v@(Var _) = return v

liftLambda :: (Monad m, MonadGenBnd m) => Expression -> m Expression
liftLambda e = do
  let ((Var (Sf recur _)) : (lamExpr : callArgs) ) = fromApplyToList e
  let (callArg : []) = callArgs -- due to the previous rewrite
  (Lambda formals liftedExpr, actuals) <- lambdaLifting lamExpr
  let (initialFormal : freeFormals) = extractBindings formals
  b <- generateBindingWith "b"
  freeArgs <- generateBindingWith "freeArgs"
  let lam = Lambda (Direct b)
            (Let (Destructure [initialFormal, freeArgs]) (Apply (Var (Sf "ohua.lang/id" Nothing)) $ Var $ Local b)
              (Let (Destructure freeFormals) (Apply (Var (Sf "ohua.lang/id" Nothing)) $ Var $ Local freeArgs)
                liftedExpr))
  return $ Apply
            (Apply
              (Apply recur_sf lam)
              callArg)
            $ fromListToApply (Sf ALangRefs.array Nothing) $ map (Var . Local) actuals


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
