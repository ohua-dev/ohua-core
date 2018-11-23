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

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.ALang.Passes.Control (liftIntoCtrlCtxt)
import qualified Ohua.ALang.Refs as Refs (ctrl, ifThenElse, not, scope, select)
import Ohua.ALang.Util (fromListToApply, lambdaLifting, mkDestructured)
import Ohua.Unit

import Control.Monad (foldM)

ifSf :: Expression
ifSf = Var $ Sf Refs.ifThenElse Nothing

ctrlSf :: Expression
ctrlSf = Var $ Sf Refs.ctrl Nothing

selectSf :: Expression
selectSf = Var $ Sf Refs.select Nothing

ifFun :: Expression
ifFun = Var $ Sf "ohua.lang/ifFun" Nothing

ifRewrite :: (Monad m, MonadGenBnd m) => Expression -> m Expression
ifRewrite (Let v a b) = Let v <$> ifRewrite a <*> ifRewrite b
ifRewrite (Lambda v e) = Lambda v <$> ifRewrite e
ifRewrite (Apply (Apply (Apply ifSf cond) trueBranch) falseBranch) = do
    ctrlTrue <- generateBindingWith "ctrlTrue"
    ctrlFalse <- generateBindingWith "ctrlFalse"
    trueBranch' <- liftIntoCtrlCtxt ctrlTrue trueBranch
    falseBranch' <- liftIntoCtrlCtxt ctrlFalse falseBranch
    -- return $
    --     [ohualang|
    --       let ($var:ctrlTrue, $var:ctrlFalse) = ohua.lang/ifFun $var:cond in
    --         let trueResult = $expr:trueBranch' in
    --          let falseResult = $expr:falseBranch' in
    --           let result = ohua.lang/select cond trueResult falseResult in
    --             result
    --                |]
    ctrls <- generateBindingWith "ctrls"
    ctrlTrue <- generateBindingWith "ctrlTrue"
    ctrlFalse <- generateBindingWith "ctrlFalse"
    trueResult <- generateBindingWith "trueResult"
    falseResult <- generateBindingWith "falseResult"
    result <- generateBindingWith "result"
    Let (Direct ctrls) (Apply ifFun cond) <$>
        (mkDestructured [ctrlTrue, ctrlFalse] ctrls $
         Let (Direct trueResult) trueBranch' $
         Let (Direct falseResult) falseBranch' $
         Let
             (Direct result)
             (Apply (Apply (Apply selectSf cond) $ Var $ Local trueResult) $
              Var $ Local falseResult) $
         Var $ Local result)
