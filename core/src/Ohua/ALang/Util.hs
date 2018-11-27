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

import Ohua.ALang.Lang
import qualified Ohua.ALang.Refs as Refs (nth)

import Data.Functor.Foldable
import Data.Generics.Uniplate.Direct (transform, universe)
import qualified Data.HashSet as HS
import qualified Data.Text as T

substitute :: Binding -> Expression -> Expression -> Expression
-- Postwalk avoids an infinite recursion in a case where `val` uses a
-- `var` binding.  This should never happen but might if this
-- invariant is violated for some reason and the violation is not
-- caught.
substitute !var val =
    transform $ \case
        Var v
            | v == var -> val
        other -> other
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

-- FIXME lambda lifting for an expression that is not a lambda should also work!
--       it just creates the lambda!
lambdaLifting ::
       (Monad m, MonadGenBnd m) => Expression -> m (Expression, [Binding])
lambdaLifting o@(Lambda _ _) =
    let (formalVars, _) = lambdaArgsAndBody o
        freeVars = findFreeVariables o
        actuals = sort $ toList freeVars -- makes the list of args deterministic
     in case actuals of
            [] -> return (o, [])
            _ -> do
                newFormals <- mapM generateBindingWith actuals
                let rewrittenExp = foldl renameVar o $ zip actuals newFormals
                return
                    (mkLambda (formalVars ++ newFormals) rewrittenExp, actuals)
lambdaLifting e =
    error $
    T.pack $
    "Invariant broken! Lambda lifting is only applicable to lambda expressions! Found: " ++
    (show e)

mkLambda :: [Binding] -> Expression -> Expression
mkLambda args expr = go expr $ reverse args
  where
    go e (a:as) = flip go as $ Lambda a e
    go e [] = e

renameVar :: Expression -> (Binding, Binding) -> Expression
renameVar e (old, new) =
    flip transform e $ \case
        Var v
            | v == old -> Var new
        other -> other

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

fromListToApply :: FunRef -> [Expr] -> Expr
fromListToApply f (v:[]) = Apply (Lit $ FunRefLit f) v
fromListToApply f (v:vs) = Apply (fromListToApply f vs) v

fromApplyToList :: Expr -> (FunRef, [Expr])
fromApplyToList (Apply (Lit (FunRefLit f)) v) = (f, [v])
fromApplyToList (Apply a b) =
    let (f, args) = fromApplyToList a
     in (f, args ++ [b])

mkDestructured :: [Binding] -> Binding -> Expression -> Expression
mkDestructured formals compound e = do
    let lastIdx = (length formals) - 1
    foldl go e $ reverse $ zip formals [0 .. lastIdx]
  where
    go :: Expression -> (Binding, Int) -> Expression
    go expr (f, i) =
        Let
            f
            (Apply
                 (Apply (Sf Refs.nth Nothing) $ Lit $ NumericLit $ toInteger i) $
             Var compound)
            expr

lambdaArgsAndBody :: Expression -> ([Binding], Expression)
lambdaArgsAndBody (Lambda arg l@(Lambda _ _)) =
    let (args, body) = lambdaArgsAndBody l
     in (args ++ [arg], body)
lambdaArgsAndBody (Lambda arg body) = ([arg], body)
