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
import Ohua.Types

import Control.Comonad
import Data.Functor.Foldable (embed, para)
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
    mkNthExpr idx source0 =
        PureFunction Refs.nth Nothing `Apply` (Lit $ NumericLit idx) `Apply` source0

lambdaLifting ::
       (Monad m, MonadGenBnd m) => Expression -> m (Expression, [Expression])
lambdaLifting e = do
    (e', actuals) <- go findFreeVariables renameVar bindingFromVar e
    (e'', actuals') <- go findLiterals replaceLit bindingFromLit e'
    return (e'', actuals ++ actuals')
  where
    go :: (Monad m, MonadGenBnd m)
       => (Expression -> [Expression])
       -> (Expression -> (Expression, Binding) -> Expression)
       -> (Expression -> m Binding)
       -> Expression
       -> m (Expression, [Expression])
    go findFreeExprs renameExpr toBinding expr =
        let (formalVars, b) =
                case expr of
                    (Lambda _ _) -> lambdaArgsAndBody expr
                    _ -> ([], expr)
            actuals = findFreeExprs expr
         in case actuals of
                [] -> return (expr, [])
                _ -> do
                    newFormals <- mapM toBinding actuals
                    let rewrittenExp =
                            foldl renameExpr b $ zip actuals newFormals
                    return
                        ( mkLambda (formalVars ++ newFormals) rewrittenExp
                        , actuals)
    bindingFromVar (Var v) = generateBindingWith v
    bindingFromLit (Lit (NumericLit l)) =
        generateBindingWith $ "lit_" <> (show l)
    bindingFromLit (Lit UnitLit) = generateBindingWith "lit_unit"

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

fromListToApply :: FunRef -> [Expr] -> Expr
fromListToApply f args = go $ reverse args
  where
    go (v:[]) = Apply (Lit $ FunRefLit f) v
    go (v:vs) = Apply (go vs) v

fromApplyToList :: Expr -> (FunRef, [Expr])
fromApplyToList =
    para $ \case
        ApplyF (extract -> (f, args)) (arg, _) -> (f, args ++ [arg])
        LitF (FunRefLit f) -> (f, [])
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
