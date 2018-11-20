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

The translation of `if` itself then produces the following code:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let tBranchCtrl :: Control Bool = ctrl cond in
      let fBranchCtrl :: Control Bool = ctrl ncond in
        let resultTrue :: Control a = (\() -> ... true branch expression ... ) tBranchCtrl in
          let resultFalse :: Control a = (\() -> ... false branch expression ... ) fBranchCtrl in
            let result :: a = select tBranchCtrl fBranchCtrl resultTrue resultFalse in
              result
@

Currently, we can perform this transformation without type annotiations because of the following reasons:

  1. The `Bool` type of `cond` is verify by the `not` function which requires a `Bool`.
  2. The `Control` types can be easily found via checking the source of the local, i.e., the binding that created the local.

Semantically, each branch runs in a `Control` context and as such that resulting value depends on
control value applied to this computation. The `select` then retrieves the final result using both control signals.

We finally simplify the `select` known that

  1. The `ctrl` calls will be removed by the lowering pass because its inputs are already of type `Bool`.
  2. The `fBranchCtrl` value is the negation of the `tBranchCtrl` value.

As such, we write:

@
let result :: a = select cond resultTrue resultFalse in ...
@

Note that we translate the applications `(\() -> ... branch ...) ctrlVal` into the following:

@
let x = scope ctrl a b in
 ...
  let y = idependentFn ctrl in
   ...
@

In fact, this applies the control value to the lambda expression.
As a result, we can write the following:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let tBranchCtrl :: Control Bool = ctrl cond in
      let fBranchCtrl :: Control Bool = ctrl ncond in
        let resultTrue :: Control a = ... true branch expression ... in
          let resultFalse :: Control a = ... false branch expression ... in
            let result :: a = select tBranchCtrl fBranchCtrl resultTrue resultFalse in
              result
@

Now this expression can be lowered to DFLang without any further ado.
The lowering itself should be sensitive to value of type `Control` and perform the
respective steps.
As a last step, we can optimize the DFLang expression to end up with:

@
let cond :: Bool = ...
  let ncond :: Bool = not cond in
    let resultTrue :: Control a = (\() -> ... true branch expression ... ) cond in
      let resultFalse :: Control a = (\() -> ... false branch expression ... ) ncond in
        let result :: a = select cond resultTrue resultFalse in
          result
@
-}

module Ohua.ALang.Passes.If where

import           Ohua.Prelude

import qualified Ohua.ALang.Refs as Refs (ifThenElse, scope, nth, not, ctrl, select)
import           Ohua.ALang.Util (fromListToApply, lambdaLifting)
import           Ohua.ALang.Lang
import           Ohua.Unit

import           Control.Monad (foldM)

-- mkIntLiteral :: Int -> m Expression
addEnvExpr = undefined

ifSf :: Expression
ifSf = Var $ Sf Refs.ifThenElse Nothing

scopeSf :: Expression
scopeSf = Var $ Sf Refs.scope Nothing

notSf :: Expression
notSf = Var $ Sf Refs.not Nothing

ctrlSf :: Expression
ctrlSf = Var $ Sf Refs.ctrl Nothing

selectSf :: Expression
selectSf = Var $ Sf Refs.select Nothing

-- TODO verify that this transformation works with the normalization passes
--      because it has a lambda in function position of a call and not anymore
--      as an argument to a HOF

ifRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
ifRewrite (Let v a b) = Let v <$> ifRewrite a <*> ifRewrite b
ifRewrite (Lambda v e) = Lambda v <$> ifRewrite e
ifRewrite (Apply (Apply (Apply ifSf cond) trueBranch) falseBranch) = do
  nCond        <- generateBinding
  tBranchCtrl  <- generateBinding
  fBranchCtrl  <- generateBinding
  resultTrue   <- generateBinding
  resultFalse  <- generateBinding
  result       <- generateBinding
  trueBranch'  <- (liftBranchIntoCtrlCtxt tBranchCtrl) <$> lambdaLiftBranch trueBranch
  falseBranch' <- (liftBranchIntoCtrlCtxt fBranchCtrl) <$> lambdaLiftBranch falseBranch
  return $ Let (Direct nCond) (Apply notSf cond)
          $ Let (Direct tBranchCtrl) (Apply ctrlSf cond)
           $ Let (Direct fBranchCtrl) (Apply ctrlSf $ Var $ Local nCond)
            $ Let (Direct resultTrue) (Apply trueBranch' $ Var $ Local tBranchCtrl)
             $ Let (Direct resultFalse) (Apply falseBranch' $ Var $ Local fBranchCtrl)
              $ Let (Direct result) (Apply (Apply (Apply selectSf cond) $ Var $ Local resultTrue) $ Var $ Local resultFalse)
               $ Var $ Local result

lambdaLiftBranch :: (Monad m, MonadGenBnd m) => Expression -> m Expression
lambdaLiftBranch branch@(Lambda args _) = do
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

liftBranchIntoCtrlCtxt :: Binding -> Expression -> Expression
liftBranchIntoCtrlCtxt ctrl (Lambda args body) = addCtrl body
  where
    addCtrl (Let v e ie) = Let v (addCtrl e) (addCtrl ie)
    addCtrl v@(Var _) = v
    addCtrl l@(Lambda _ _) = l -- we are after normalization so all existing lambdas are from other HOFs
    addCtrl (Apply f@scopeSf arg) = Apply (Apply f $ Var $ Local ctrl) arg
    addCtrl (Apply f@(Var (Sf _ _)) someUnitExpr) = Apply f $ Var $ Local ctrl
    addCtrl (Apply f arg) = flip Apply arg $ addCtrl arg
