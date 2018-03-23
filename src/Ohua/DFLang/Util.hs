module Ohua.DFLang.Util where

import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Sequence

import Ohua.DFLang.Lang
import Ohua.Types

-- | Find the usages of a binding
findUsages :: Binding -> Seq LetExpr -> [LetExpr]
findUsages binding =
    toList . Data.Sequence.filter (elem (DFVar binding) . callArguments)

-- | Find the definition of a binding
findDefinition :: Binding -> Seq LetExpr -> Maybe LetExpr
findDefinition binding = find (g . returnAssignment)
  where
    g (Direct b) = b == binding
    g (Destructure bindings) = binding `elem` bindings
    g (Recursive r) = r == binding

-- | Find the first call site of an expression by function reference.
findExpr :: DFFnRef -> Seq LetExpr -> Maybe LetExpr
findExpr fnRef = find ((== fnRef) . functionRef)

-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)

-- | Find the free variables inside a set of expressions (i.e. a lambda expression).
findFreeVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findFreeVars exprs = flip findFreeVars0 exprs $ findBoundVars exprs

findFreeVars0 ::
       Foldable f => HS.HashSet Binding -> f LetExpr -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . concatMap (mapMaybe f . callArguments)
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
