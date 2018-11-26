module Ohua.DFLang.Passes.TailRec where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util

import Data.Sequence ((><))

recurLowering :: DFExpr -> DFExpr
recurLowering (DFExpr letExprs returnVar)
  -- 1. Find the recurFun with two outputs
 =
    let recurFuns =
            filter ((2 ==) . length . extractBindings . returnAssignment) $
            toList $ findAllExprs Refs.recurFun letExprs
  -- 2. traverse the subtree to find the corresponding recurFun with only a single output
        pairs =
            flip map recurFuns $ \recurFunStart ->
                let recurFunEnd = findEnd $ anySuccessor recurFunStart
                 in (recurFunStart, recurFunEnd)
        finalRecurFuns =
            flip map pairs $ \(recurFunStart, recurFunEnd) ->
                let (fixRef:(cond:recurArgs)) = callArguments recurFunEnd
                 in LetExpr
                        (callSiteId recurFunStart)
                        (Destructure $
                         (extractBindings $ returnAssignment recurFunStart) ++
                         (extractBindings $ returnAssignment recurFunEnd))
                        (functionRef recurFunStart)
                        [(callArguments recurFunStart), fixRef, cond, recurArgs]
                        Nothing
     in finalRecurFuns >< removeAllExprs recurFuns letExprs
  where
    findEnd LetExpr {functionRef = f}
        | f == Refs.recurFun = letExpr
    -- all paths lead to the final recurFun because this is a connected component where
    -- the recurFun at the very end has the only outgoing arc.
    findEnd = findEnd . anySuccessor
    anySuccessor = head . findUsages . head . extractBindings . returnAssignment
