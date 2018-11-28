module Ohua.DFLang.Util where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Sequence as DS

import Ohua.DFLang.Lang

-- | Find the usages of a binding
findUsages :: Binding -> Seq LetExpr -> [LetExpr]
findUsages binding = toList . DS.filter (elem (DFVar binding) . callArguments)

-- | Find the definition of a binding
findDefinition :: Binding -> Seq LetExpr -> Maybe LetExpr
findDefinition binding = find ((binding `elem`) . output)

-- | Find the first call site of an expression by function reference.
findExpr :: DFFnRef -> Seq LetExpr -> Maybe LetExpr
findExpr fnRef = find ((== fnRef) . functionRef)

findAllExprs :: DFFnRef -> Seq LetExpr -> Seq LetExpr
findAllExprs fnRef = DS.filter ((== fnRef) . functionRef)

removeAllExprs :: Seq LetExpr -> Seq LetExpr -> Seq LetExpr
removeAllExprs toRemove allExprs =
    let t = HS.fromList $ toList $ map functionRef toRemove
     in foldl
            (\s e ->
                 if HS.member (functionRef e) t
                     then s
                     else s |> e)
            DS.empty
            allExprs
