module Ohua.DFLang.TailRec where

import Ohua.Types
import Ohua.DFLang.HOF
import Ohua.DFLang.Lang
import Ohua.DFLang.Refs

import Control.Monad.Except
import Control.Monad.State
import Control.Exception

import           Data.Foldable
import           Data.Sequence        (Seq, (><))
import qualified Data.Sequence        as S



-- the call in ALang is still (recur algoRef args).
-- it needs to become (recur algoRef conditionOutput algoInArgs recurArgs).
-- it is not possible to perform this kind of transformation using the HOF type class.

-- FIXME this needs to be a lowering pass from ALang to DFLang for Letrecs

transformRecursiveTailCall :: DFExpr -> DFExpr
transformRecursiveTailCall dfExprs = handleRecursiveTailCall dfExprs $ findExpr (DFFunction "ohua.lang/recur") dfExprs

handleRecursiveTailCall :: DFExpr -> Maybe LetExpr -> DFExpr
handleRecursiveTailCall dfExprs Nothing = dfExprs
handleRecursiveTailCall dfExprs (Just recurFn) =
    let
        -- get the condition result
        unAssignment x = case x of
                              Direct b -> b
                              _ -> error "invariant broken: recur always gives direct output"
        usages = findUsages (unAssignment (returnAssignment recurFn)) dfExprs
        switchFn = find ((== DFFunction "ohua.lang/switch") . functionRef) usages
        conditionOutput :: DFVar
        conditionOutput = case switchFn of
                               Just (LetExpr _ _ _ (c:_) _) -> c
                               Nothing -> error "invariant broken: recur is assumed to be the final call on a 'then' or 'else' branch."
        -- get the algo-in
        -- I'm using the explicit reference here because finding the proper algo-in by backwards graph traversal needs to understand the scope ops!


    in dfExprs
