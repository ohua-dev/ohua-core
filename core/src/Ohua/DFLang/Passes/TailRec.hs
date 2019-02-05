{-# language PartialTypeSignatures #-}
module Ohua.DFLang.Passes.TailRec where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import qualified Ohua.ALang.Passes.TailRec as ALangPass

import qualified Data.List.NonEmpty as NE
import Data.Sequence as DS ((><), filter)

recurLowering :: DFExpr -> DFExpr
recurLowering (DFExpr letExprs returnVar)
  -- 1. Find the recurFun with two outputs
 =
    flip DFExpr returnVar $
    DS.filter ((/= ALangPass.recurEndMarker) . nodeRef . functionRef) $
    fmap
        (\recurStart ->
             if nodeRef (functionRef recurStart) /= ALangPass.recurStartMarker
                 then recurStart
                 else assert (length (output recurStart) == 2) $
                      let endFunction = findEnd $ allSuccessors recurStart
                          fixRef:cond:recurArgs = callArguments endFunction
                       in recurStart
                              { output = output recurStart <> output endFunction
                              , functionRef = Refs.recurFun
                              -- FIXME we don't need the var lists when we use the assertion
                              -- that these two lists have the same size! and this is always
                              -- true because these are the arguments to a call to the same
                              -- function, i.e, the recursion!
                              , callArguments =
                                    fixRef : cond : callArguments recurStart <> recurArgs
                              })
        letExprs
  where
    findEnd l@(LetExpr {functionRef = f})
        | nodeRef f == ALangPass.recurEndMarker = l
    -- all paths lead to the final recurFun because this is a connected component where
    -- the recurFun at the very end has the only outgoing arc.
    findEnd e = findEnd $ allSuccessors e
    head (x:_) = x
    head _ = error "head"
    allSuccessors = head . flip findUsages letExprs . head . output
