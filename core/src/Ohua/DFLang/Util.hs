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

-- TODO: remove all the code below once the function `Ohua.DFLang.Passes.lowerHOF` is gone
-- | Find all locally bound variables.
findBoundVars :: (Container c, Element c ~ LetExpr) => c -> HS.HashSet Binding
findBoundVars = HS.fromList . concatMap output . toList

-- | Find the free variables inside a set of expressions (i.e. a lambda expression).
findFreeVars :: (Container c, Element c ~ LetExpr) => c -> HS.HashSet Binding
findFreeVars exprs = flip findFreeVars0 exprs $ findBoundVars exprs

findFreeVars0 ::
       (Container c, Element c ~ LetExpr)
    => HS.HashSet Binding
    -> c
    -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . foldMap (mapMaybe f . callArguments)
  where
    f (DFVar b)
        | not (HS.member b boundVars) = Just b
    f _ = Nothing

-- | Rename the free variables listed in the mapping.
renameWith :: Functor f => HM.HashMap Binding Binding -> f LetExpr -> f LetExpr
renameWith m = fmap go
  where
    go e =
        e
            { callArguments =
                  map
                      (\case
                           v@(DFVar var) -> maybe v DFVar $ HM.lookup var m
                           v -> v)
                      (callArguments e)
            }
