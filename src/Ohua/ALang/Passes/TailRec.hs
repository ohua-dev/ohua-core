module Ohua.ALang.Passes.TailRec where

import Ohua.ALang.Lang
import Ohua.Types
import qualified Data.HashSet as HS

--
-- This must run before algos are being inlined!
--

-- TODO verify the following assumption:
--      the recursive call is the last expression on a if branch (lambda expression used in if).

-- the call is (recur algoRef args)
findTailRec :: Expression -> HS.HashSet Binding -> Expression
-- definition of an algo
findTailRec (Let (Direct a) expr inExpr) algosInScope = Let (Direct a) (findTailRec expr i) (findTailRec inExpr i)
    where i = HS.insert a algosInScope
findTailRec (Let a expr inExpr) algosInScope = Let a (findTailRec expr algosInScope) (findTailRec inExpr algosInScope)
-- lifted lambda
findTailRec (Apply (Var (Local binding)) a) algosInScope | HS.member binding algosInScope =
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
    Apply (Apply "ohua.lang/recur" (Var (Local binding))) a
findTailRec (Apply a b) algosInScope = Apply (findTailRec a algosInScope) (findTailRec b algosInScope)
findTailRec (Var b) _ = Var b
findTailRec (Lambda a e) algosInScope = Lambda a $ findTailRec e algosInScope

