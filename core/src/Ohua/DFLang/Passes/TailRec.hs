module Ohua.DFLang.Passes.TailRec where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util

import qualified Data.List.NonEmpty as NE
import Data.Sequence as DS ((><), filter)

recurLowering :: DFExpr -> DFExpr
recurLowering (DFExpr letExprs returnVar)
  -- 1. Find the recurFun with two outputs
 =
    let recurFuns =
            DS.filter ((2 ==) . length . output) $
            findAllExprs Refs.recurFun letExprs
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
                        ((output recurFunStart) ++ (output recurFunEnd))
                        (functionRef recurFunStart)
                        Nothing
                        -- FIXME we don't need the var lists when we use the assertion
                        -- that these two lists have the same size! and this is always
                        -- true because these are the arguments to a call to the same
                        -- function, i.e, the recursion!
                        [ DFVarList
                              (join $
                               map extractBindings $ callArguments recurFunStart)
                        , fixRef
                        , cond
                        , DFVarList (join $ map extractBindings recurArgs)
                        ]
     in flip DFExpr returnVar $
        finalRecurFuns >< removeAllExprs recurFuns letExprs
  where
    findEnd l@(LetExpr {functionRef = f})
        | f == Refs.recurFun = l
    -- all paths lead to the final recurFun because this is a connected component where
    -- the recurFun at the very end has the only outgoing arc.
    findEnd e = (findEnd . anySuccessor) e
    anySuccessor =
        head .
        NE.fromList . (flip findUsages letExprs) . head . NE.fromList . output
    extractBindings (DFEnvVar _) = []
    extractBindings (DFVar b) = [b]
    extractBindings (DFVarList bs) = bs
