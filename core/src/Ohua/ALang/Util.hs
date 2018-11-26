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

import           Ohua.Prelude
import           Ohua.ALang.Lang
import qualified Ohua.ALang.Refs as Refs

import           Data.Functor.Foldable
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Generics.Uniplate.Direct (transform, universe)



substitute :: Binding -> Expression -> Expression -> Expression
-- Postwalk avoids an infinite recursion in a case where `val` uses a
-- `var` binding.  This should never happen but might if this
-- invariant is violated for some reason and the violation is not
-- caught.
substitute !var val = transform $ \case Var v | v == var -> val; other -> other
substitute !var val = cata f
  where
    f (VarF v)
        | var == v = val
    f e = embed e
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

-- | Note that this only performs the initial lifting of the lambda expression.
-- It does not find calls to the lambda and rewrites them accordingly.
-- This is due to the fact that we often want to package the freeVars via a call to
-- ohua.lang/array to make the backend implementation easier and I'm not sure whether
-- this is always true.
-- This is also the readon why I keep this in Util into of making it an own pass.

destructure :: Expr -> [Binding] -> Expr -> Expr
destructure source bnds =
    foldl (.) id $
    map (\(idx, bnd0) -> Let bnd0 $ mkNthExpr idx source) (zip [0 ..] bnds)
  where
    mkNthExpr idx source =
        Sf Refs.nth Nothing `Apply` (Lit $ NumericLit idx) `Apply` source

lambdaLifting :: (Monad m, MonadGenBnd m) => Expression -> m (Expression, [Binding])
lambdaLifting o@(Lambda v e) =
  let freeVars  = HS.delete v (findFreeVariables e)
      actuals   = sort $ HS.toList freeVars -- makes the list of args deterministic
  in case actuals of
      [] -> return (o,[])
      _  -> do
          formals <- mapM generateBindingWith actuals
          let rewrittenExp = foldl renameVar e $ zip actuals formals
          input <- generateBinding
          return (Lambda input $ destructure (Var input) (v : formals) rewrittenExp, actuals)
lambdaLifting e = error $ T.pack $ "Invariant broken! Lambda lifting is only applicable to lambda expressions! Found: " ++ (show e)

renameVar :: Expression -> (Binding, Binding) -> Expression
renameVar e (old, new) = flip transform e $ \case Var v | v == old -> Var new; other -> other

-- | All bindings defined in an expression *with duplicates*
definedBindings :: Expression -> [Binding]
definedBindings e =
    [ v
    | e' <- universe e
    , v <-
          case e of
              Let v' _ _ -> [v']
              Lambda v' _ -> [v']
              _ -> []
    ]

-- | A very simple function that calculates all bindings that are used in an
-- expression but not defined in it. This is implemented as a simple set
-- intersection, therefore it relies on the fact that the expression is in SSA
-- form.
findFreeVariables :: Expression -> HS.HashSet Binding
findFreeVariables e =
    HS.difference
        (HS.fromList [v | Var v <- universe e])
        (HS.fromList $ definedBindings e)
