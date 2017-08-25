module Ohua.ALang.Passes.TailRec where

import Ohua.ALang.Lang
import Ohua.Types
import qualified Data.HashSet as HS

--
-- This must run before algos are being inlined!
--

-- TODO verify the following assumption:
--      the recursive call is the last expression on an if branch (lambda expression used in if).
--      if this turns out not to be true then we should throw an exception.

-- the call is (recur algoRef args)
findTailRecs :: Expression -> Expression
findTailRecs  = snd . flip findRecCall HS.empty

findRecCall :: Expression -> HS.HashSet Binding -> (HS.HashSet Binding, Expression)
findRecCall (Let (Direct a) expr inExpr) algosInScope =
  let (found, e) = findRecCall expr $ HS.insert a algosInScope in
      case found of _ | HS.member a found -> let (iFound, iExpr) = findRecCall inExpr algosInScope in
                                                 (iFound, Let (Recursive a) e iExpr)
                    -- this is supposed to cover the following case:
                    -- Let x $ Lambda ... Let a (Lambda ... Apply x Var "bla") ... Apply a Var "blub"
                    _ | HS.size found > 0 -> let (iFound, iExpr) = findRecCall inExpr $ HS.insert a algosInScope in
                                                 (HS.union found $ HS.delete a iFound, Let (Recursive a) e iExpr)
                    _                     -> let (iFound, iExpr) = findRecCall inExpr algosInScope in
                                                 (iFound, Let (Direct a) e iExpr)
findRecCall (Let a expr inExpr) algosInScope =
  let (iFound, iExpr) = findRecCall inExpr algosInScope in
      (iFound, Let a expr iExpr)
findRecCall (Apply (Var (Local binding)) a) algosInScope | HS.member binding algosInScope =
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
    (HS.insert binding HS.empty, Apply (Apply "ohua.lang/recur" (Var (Local binding))) a)
findRecCall (Apply a b) algosInScope =
  let (aFound, aExpr) = findRecCall a algosInScope
      (bFound, bExpr) = findRecCall b algosInScope in
      (HS.union aFound bFound, Apply aExpr bExpr)
findRecCall (Var b) _ = (HS.empty, Var b)
findRecCall (Lambda a e) algosInScope =
  let (eFound, eExpr) = findRecCall e algosInScope in
      if HS.size eFound == 0 then (eFound, Lambda a eExpr)
                              -- TODO would I need to lift that lambda here?
                             else (eFound, Lambda a eExpr)
