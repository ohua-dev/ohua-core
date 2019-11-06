-- |
-- Module      : $Header$
-- Description : Utilities for working with the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}

module Ohua.ALang.Util where

import Ohua.Prelude

import Ohua.ALang.Lang
import qualified Ohua.ALang.Refs as Refs (nth)

import Control.Comonad
import qualified Control.Lens as Lens (para)
import Data.Functor.Foldable (embed, para)
import qualified Data.HashSet as HS
import qualified Data.List as L
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

-- | Note that this only performs the initial lifting of the lambda expression.
-- It does not find calls to the lambda and rewrites them accordingly.
-- This is due to the fact that we often want to package the freeVars via a call to
-- ohua.lang/array to make the backend implementation easier and I'm not sure whether
-- this is always true.
-- This is also the reason why I keep this in Util into of making it an own pass.
destructure :: Expr -> [Binding] -> Expr -> Expr
destructure source bnds =
    foldl (.) id $
    map (\(idx, bnd0) -> Let bnd0 $ mkNthExpr idx source) (zip [0 ..] bnds)
  where
    mkNthExpr idx source0 =
        PureFunction Refs.nth Nothing `Apply` (Lit $ NumericLit idx) `Apply`
        (Lit $ NumericLit $ toInteger $ length bnds) `Apply`
        source0

lambdaLifting ::
       (Monad m, MonadGenBnd m) => Expression -> m (Expression, [Expression])
lambdaLifting e = do
    (e', actuals) <- go findFreeVariables e
    (e'', actuals') <- go findLonelyLiterals e'
    return (e'', actuals ++ actuals')
  where
    go :: (Monad m, MonadGenBnd m)
       => (Expression -> [Expression])
       -> Expression
       -> m (Expression, [Expression])
    go findFreeExprs expr
        | null actuals = pure (expr, [])
        | otherwise = do
            newFormals <- mapM (bindingFromAny) actuals
            let rewrittenExp =
                    foldl
                        (\e (from, to) -> renameExpr from (Var to) e)
                        b
                        (zip actuals newFormals)
            return (mkLambda (formalVars ++ newFormals) rewrittenExp, actuals)
      where
        (formalVars, b) =
            case expr of
                (Lambda _ _) -> lambdaArgsAndBody expr
                _ -> ([], expr)
        actuals = findFreeExprs expr
        renameExpr from to =
            rewrite $ \e ->
                if e == from
                    then Just to
                    else Nothing
    bindingFromAny (Var v) = generateBindingWith v
    bindingFromAny (Lit l) = generateBindingWith $ "lit_" <> litType
      where
        litType =
            case l of
                NumericLit l -> show l
                UnitLit -> "unit"
                FunRefLit ref -> bindifyFunRef ref
                EnvRefLit l -> "env_" <> show l
    bindifyFunRef :: FunRef -> Binding
    bindifyFunRef _ = "fun_ref" -- TODO

mkLambda :: [Binding] -> Expression -> Expression
mkLambda args expr = go expr $ reverse args
  where
    go e (a:as) = flip go as $ Lambda a e
    go e [] = e

replaceLit :: Expression -> (Expression, Binding) -> Expression
replaceLit e (Lit old, new) =
    flip transform e $ \case
        Lit l
            | l == old -> Var new
        other -> other

renameVar :: Expression -> (Expression, Binding) -> Expression
renameVar e (Var old, new) =
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
          case e' of
              Let v' _ _ -> [v']
              Lambda v' _ -> [v']
              _ -> []
    ]

-- | A very simple function that calculates all bindings that are used in an
-- expression but not defined in it. This is implemented as a simple set
-- intersection, therefore it relies on the fact that the expression is in SSA
-- form.
findFreeVariables :: Expression -> [Expression]
findFreeVariables e =
    map Var $
    sort $ -- makes the list of args deterministic
    toList $
    HS.difference
        (HS.fromList [v | Var v <- universe e])
        (HS.fromList $ definedBindings e)

findLiterals :: Expression -> [Expression]
findLiterals e =
    [ Lit lit
    | Lit l <- universe e
    , lit <-
          case l of
              UnitLit -> [l]
              NumericLit _ -> [l]
              EnvRefLit _ -> [l]
              _ -> []
    ]

-- | A literal is lonely if it does not accompany a var in the argument list to a call.
findLonelyLiterals :: HasCallStack => Expression -> [Expression]
findLonelyLiterals =
    Lens.para $ \case
        f@Apply {} ->
            const $
            if areAllLits args
                then take 1 $
                     -- HACK see #34
                     filter
                         (\case
                              Lit (FunRefLit _) -> False
                              _ -> True)
                         args
                -- We could also return `[]` in the else branch, because the
                -- expression should be normalized, but this is cleaner
                else args >>= findLonelyLiterals
            where args = getFunctionArgs f --snd $ fromApplyToList f
        _ -> join
  where
    areAllLits =
        all $ \case
            Lit _ -> True
            _ -> False

mkApply :: Expr -> [Expr] -> Expr
mkApply f args = go $ reverse args
  where
    go (v:[]) = Apply f v
    go (v:vs) = Apply (go vs) v
    go [] = f

fromListToApply :: FunRef -> [Expr] -> Expr
fromListToApply f = mkApply $ Lit $ FunRefLit f

getFunctionArgs :: HasCallStack => Expr -> [Expr]
getFunctionArgs e = args
  where
    (_, _, args) = fromApplyToList' e

fromApplyToList :: HasCallStack => Expr -> (FunRef, [Expr])
fromApplyToList e =
    case state of
        Just s ->
            error $ "Expected pure function, but found bound state: " <> show s
        _ -> (f, args)
  where
    (f, state, args) = fromApplyToList' e

fromApplyToList' :: HasCallStack => Expr -> (FunRef, Maybe Expr, [Expr])
fromApplyToList' =
    para $ \case
        ApplyF (extract -> (f, s, args)) (arg, _) -> (f, s, args ++ [arg])
        LitF (FunRefLit f) -> (f, Nothing, [])
        BindStateF (state, _) (method, _) ->
            case method of
                Lit (FunRefLit f) -> (f, Just state, [])
                other ->
                    error $
                    "Expected state to be bound to function, found: " <>
                    show other
        other ->
            error $
            "Expected apply or function reference, got: " <>
            show (embed $ fmap fst other)

mkDestructured :: [Binding] -> Binding -> Expression -> Expression
mkDestructured formals compound = destructure (Var compound) formals

lambdaArgsAndBody :: Expression -> ([Binding], Expression)
lambdaArgsAndBody (Lambda arg l@(Lambda _ _)) =
    let (args, body) = lambdaArgsAndBody l
     in ([arg] ++ args, body)
lambdaArgsAndBody (Lambda arg body) = ([arg], body)
lambdaArgsAndBody e = ([], e)
