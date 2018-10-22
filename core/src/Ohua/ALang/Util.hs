-- |
-- Module      : $Header$
-- Description : Utilities for working with the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Util where

import Ohua.Prelude

import           Data.Functor.Foldable
import qualified Data.HashSet as HS

import           Ohua.ALang.Lang


substitute :: Binding -> Expression -> Expression -> Expression
-- Postwalk avoids an infinite recursion in a case where `val` uses a
-- `var` binding.  This should never happen but might if this
-- invariant is violated for some reason and the violation is not
-- caught.
substitute !var val = cata f
  where
    f (VarF (Local v)) | var == v = val
    f e                = embed e
-- substitute var val e =
--     case e of
--         Var (Local v)
--             | var == v -> val
--             | otherwise -> e
--         Var _ -> e
--         Apply expr1 expr2 -> Apply (recurse expr1) (recurse expr2)
--         Let bnd expr1 expr2
--             | var `elem` flattenAssign bnd -> e
--             | otherwise -> Let bnd (recurse expr1) (recurse expr2)
--         Lambda assignment body
--             | var `elem` flattenAssign assignment -> body
--             | otherwise -> Lambda assignment (recurse body)
--   where
--     recurse = substitute var val

findFreeVariables :: Expression -> HS.HashSet Binding
findFreeVariables (Let (Direct v) e ie) =
  let ves  = findFreeVariables e
      vies = findFreeVariables ie
      in HS.union ves $ HS.delete v vies
findFreeVariables (Let (Destructure vs) e ie) =
  let ves  = findFreeVariables e
      vies = findFreeVariables ie
      in HS.union ves $ HS.difference vies $ HS.fromList vs
findFreeVariables (Let (Recursive _) _ _) = error "Invariant broken! (Recursive binding detected.)"
findFreeVariables (Apply a b) = HS.union (findFreeVariables a) $ findFreeVariables b
findFreeVariables (Var (Local v)) = HS.singleton v
findFreeVariables (Var _) = HS.empty
findFreeVariables (Lambda v e) = HS.difference (findFreeVariables e) $ HS.fromList $ extractBindings v
