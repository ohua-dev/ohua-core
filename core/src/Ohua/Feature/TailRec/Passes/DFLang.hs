{-# LANGUAGE PartialTypeSignatures #-}

module Ohua.Feature.TailRec.Passes.DFLang where

import Ohua.Prelude

import Ohua.DFLang.Lang
import Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import qualified Ohua.Feature.TailRec.Passes.ALang as ALangPass

import Data.Sequence as DS (fromList)

recurLowering :: DFExpr -> DFExpr
recurLowering (DFExpr letExprs returnVar)
  -- 1. Find the recurFun with two outputs
 =
    flip DFExpr returnVar $
    DS.fromList $ transform transformator (toList letExprs)
  where
    transformator [] = []
    transformator l@(recurStart:rest)
        | nodeRef (functionRef recurStart) /= ALangPass.recurStartMarker = l
        | otherwise =
            assert (length (output recurStart) == 2) $
            recurStart
                { output = output recurStart <> output endFunction
                , functionRef = Refs.recurFun
                              -- FIXME we don't need the var lists when we use the assertion
                              -- that these two lists have the same size! and this is always
                              -- true because these are the arguments to a call to the same
                              -- function, i.e, the recursion!
                , callArguments =
                      fixRef : cond : callArguments recurStart <> recurArgs
                } :
            delete endFunction rest
      where
        findEnd l1@(LetExpr {functionRef = f})
            | nodeRef f == ALangPass.recurEndMarker = l1
              -- all paths lead to the final recurFun because this is a connected component where
              -- the recurFun at the very end has the only outgoing arc.
        findEnd e = findEnd $ allSuccessors e
        allSuccessors = head . (flip findUsages rest <=< output)
        endFunction = findEnd $ allSuccessors recurStart
        fixRef:cond:recurArgs = callArguments endFunction
    head (x:_) = x
    head _ = error "head"
