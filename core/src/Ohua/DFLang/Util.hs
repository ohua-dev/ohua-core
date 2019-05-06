module Ohua.DFLang.Util where

import Ohua.Prelude

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as DS
import Data.Sequence (Seq, (|>))

import Ohua.DFLang.Lang

-- | Find the usages of a binding
findUsages :: Foldable f => Binding -> f LetExpr -> [LetExpr]
findUsages binding = filter (elem (DFVar binding) . callArguments) . F.toList

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
    let t = HS.fromList $ toList $ map callSiteId toRemove
     in foldl
            (\s e ->
                 if HS.member (callSiteId e) t
                     then s
                     else s |> e)
            DS.empty
            allExprs
