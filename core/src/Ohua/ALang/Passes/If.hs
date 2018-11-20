{-|
Module      : $Header$
Description : Implementation of ALang transformation for If.
Copyright   : (c) Sebastian Ertel, Justus Adam 2018. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:
We perform lambda lifting on both branches (after normalization):
@
let result = if cond
                (\() -> f a b)
                (\() -> g c d)
@
into:
@
let result = if cond
                (\() -> let x = scope a b
                         in let x0 = nth 0 x
                             in let x1 = nth 1 x
                                 in f x0 x1
                (\() -> let x = scope c d
                         in let x0 = nth 0 x
                             in let x1 = nth 1 x
                                 in g x0 x1
@
The translation of `if` itself is then done in a lowering pass from ALang to DFLang.
|-}

module Ohua.ALang.Passes.If where

import           Ohua.Prelude

import qualified Ohua.ALang.Refs as Refs (ifThenElse, scope, nth)
import           Ohua.ALang.Util (fromListToApply, lambdaLifting)
import           Ohua.ALang.Lang

import           Control.Monad (foldM)

-- mkIntLiteral :: Int -> m Expression
addEnvExpr = undefined

ifSf :: Expression
ifSf = Var $ Sf Refs.ifThenElse Nothing

ifRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
ifRewrite (Let v a b) = Let v <$> ifRewrite a <*> ifRewrite b
ifRewrite (Lambda v e) = Lambda v <$> ifRewrite e
ifRewrite (Apply (Apply ifSf trueBranch) elseBranch) = Apply <$> (Apply ifSf <$> liftBranch trueBranch)
                                                             <*> liftBranch elseBranch
  where
    liftBranch :: (Monad m, MonadGenBnd m) => Expression -> m Expression
    liftBranch branch@(Lambda args _) = do
      ((Lambda (Destructure formals) e), actuals) <- lambdaLifting branch
      x <- generateBinding
      let lastIdx = (length formals) - 1
      -- TODO turn this into a function in Util that can also be used to desugar destructuring
      ie <- foldM (mkDestructured x) e $ reverse $ zip formals [0..lastIdx]
      return $ Lambda args
                      $ Let (Direct x)
                            (fromListToApply (Sf Refs.scope Nothing) $ map (Var . Local) actuals)
                            ie
    mkDestructured :: (Monad m) => Binding -> Expression -> (Binding, Int) -> m Expression
    mkDestructured x expr (f, i) = do
                  -- idx <- addEnvExpression i -- FIXME not sure about this. see issue #17
                  idx <- addEnvExpr i -- FIXME not sure about this. see issue #17
                  return $ Let (Direct f)
                               (Apply (Apply (Var $ Sf Refs.nth Nothing) $ Var $ Env idx)
                                      $ Var $ Local x)
                               expr
