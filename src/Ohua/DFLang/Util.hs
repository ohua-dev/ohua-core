module Ohua.DFLang.Util where

import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Data.Foldable

import Ohua.DFLang.Lang
import Ohua.Types

-- | Find all locally bound variables.
findBoundVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findBoundVars = HS.fromList . fold . fmap (flattenAssign . returnAssignment)


findFreeVars :: (Functor f, Foldable f) => f LetExpr -> HS.HashSet Binding
findFreeVars exprs =  flip findFreeVars0 exprs $ findBoundVars exprs

findFreeVars0 :: Foldable f => HS.HashSet Binding -> f LetExpr -> HS.HashSet Binding
findFreeVars0 boundVars = HS.fromList . concatMap (mapMaybe f . callArguments)
  where
    f (DFVar b) | not (HS.member b boundVars) = Just b
    f _         = Nothing

renameWith :: Functor f => HM.HashMap Binding Binding -> f LetExpr -> f LetExpr
renameWith m = fmap go
  where
    go e = e { callArguments = map (\case v@(DFVar var) -> maybe v DFVar $ HM.lookup var m; v -> v;) (callArguments e) }
