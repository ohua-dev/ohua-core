{-|
Module      : $Header$
Description : Implementation of ALang transformation for If.
Copyright   : (c) Sebastian Ertel, Justus Adam 2018. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

=== Design
This design generalizes the different forms of control that existed before in Ohua, namely:

  1. [ctrl + scope for conditionals] send a value 0/1 times
  2. [oneToN for smap] send a value n times

Note that in the case of conditionals we did not actually send a value yet, but we
should which is `()`/`Unit`.

Both of the above express a form of control associated with a given context.

The generalization is as follows:
We extend `ctrl` to

@
ctrl :: Int ~ M => M -> [Vars](N) -> [[Vars](N)](M)
@

where the length of the outer list of the result is ~ `M` and the length of the inner list is `N`.
As such, `ctrl` does not return a `Bool` anymore. It is essentially the combination of the
former `ctrl` and `scope`.

For the case of smap, the new `ctrl` resembles the very same functionality that the `oneToN` encapsulated.
For the case of conditionals, we need another function that converts a `Bool` to an `Int`:

@
boolToInt True  = 1
boolToInt False = 0
@

As such, we can write the following transformation (for one of the branches):

@
let cond :: Bool = ...
  let tBranchCtrl = boolToInt cond in
    let resultTrue = (\() -> let x = ctrl tBranchCtrl a b c () in
                              ...
                               let unitVal = nth 3 x in
                                 let y = independentFn unitVal
                                   ... true branch expression ... ) in ...
@

If `tBranchCtrl` is `0` then the branch is not executed, i.e., the `ctrl` function will
drop its inputs and not produce any output.

The tip of the iceberg is now to also generalize an `smap` on a finite list (that knows its size) and an
`smap` over a sequence where the size is unknown.
The problem is this: all out-of-context variables are `ctrl` and as such need the size argument to be
propagated to the nodes of body (lambda expression)/ algorithm passed to `smap`.
Since this size may only be known at the very end or even never (in case of an endless
stream), processing inside the algorithm is stalled where the out-of-context variables are needed.

The idea is to generalize `ctrl` even further:

@
ctrl :: Int ~ M => [Var](N) -> (M -> [[Var](N)](M))
@

This extension allows us to write the following code:

@
let ctrlFun = ctrl a b c () in
  let vars :: [[Var](N)](M) = repeat ctrlFun 5 in
    ...
@

We can use this to emit the same set of out-of-context variables as often as we like to:

@
let ctrlFun = ctrl a b c () in
  smap (\d sizeRetrieved ->
          let vars :: [[Var](N)](M) = repeat ctrlFun sizeRetrieved in
            ... )
       generator
@

Now `smap` can emit any number as its `sizeRetrieved` and as such support both infinite and finite lists.
The final problem to be solved now is to support a generator nested inside another `smap`:

@
smap (\x size ->
        ...
        let ctrlFun = ctrl a b c () in
          smap (\d sizeRetrieved ->
                  let vars :: [[Var](N)](M) = repeat ctrlFun sizeRetrieved in
                    ... )
               generator
               ...)
    data
@

In other words: How does the `repeat` know when to reload its state, i.e., its `ctrlFun`?
In order to solve this final problem, the type of sizeRetrieved is as follows:

@
smapFun :: Generator -> [(Maybe Int, Data)]
@

When `repeat` receives `Nothing`, it reloads!

Implementation-wise it is clear that `repeat` needs to be an operator. We also decided to not
tear apart `ctrl` like that because in the end, we would fuse these parts anyways.
As such, we implement the following `ctrl` operator at the backend:

@
ctrl :: [Channel a] -> [Channel a] -> Channel (Maybe Int) -> ()
ctrl inChannels outChannels ctrlChannel = undefined
@

If the state is empty, then the op pulls one data item from all its incoming channels (`inChannels`).
Then it waits for input on its `ctrlChannel` and dispatches as follows:

  * [Just n] Emit the pulled data `n` times to its outputs (`outChannels`).
  * [Just 0] Send nothing.
  * [Nothing] Pull new data.

So, sending a `Nothing` will drop the pulled data.
Note that we implement `Maybe` in terms of `(Bool, Int)`.

The final transformation for `if` is then:

@
let cond :: Bool = ...
  let (ctrlTrue, ctrlFalse) = ifFun cond
    let resultTrue = (let x = ctrl ctrlTrue a b c () in
                        ...
                         let unitVal = nth 3 x in
                           let y = independentFn unitVal
                             ... true branch expression ... ) in ...
@

Again, I collapsed this a bit because we would have to fuse functions/operators anyways.
The long version is:

@
let cond :: Bool = ...
  let size = boolToInt cond
    let ctrlTrue = mkTuple cond size
      let resultTrue = (let x = ctrl ctrlTrue a b c () in
                          ...
                           let unitVal = nth 3 x in
                             let y = independentFn unitVal
                               ... true branch expression ... ) in ...
@

TODO: replace the `recur` operator in the tail recursion with a `ctrl` operator.
TODO: provide an optimization pass on DFLang that removes the `nth` nodes knowning that
the `ctrl` operator can provide this destructuring easily in its implementation.
-}
module Ohua.ALang.Passes.Control where

import Ohua.Prelude

import Ohua.ALang.Lang
import Ohua.Unit

-- | We perform the following steps:
--  1. perform lambda lifting to extract the free variables
--  2. add the `ctrl` operator with the free variables as input
--  3. provide independent functions with the unitVal that the `ctrl` operator always provides.
-- (if there are no independent function then this binding will never turn into an arc anyway.)
liftIntoCtrlCtxt :: (Monad m, MonadGenBnd m) => Binding -> Expression -> m Expression
liftIntoCtrlCtxt ctrlIn (Lambda (Destructure originalFormals) body)) = do
    ((Lambda (Destructure allFormals) e), actuals) <- lambdaLifting body
    let formals = reverse $ take (length actuals) $ reverse allFormals
    let formals' = formals ++ [unitBinding]
    unitVar <- generateBindingWith "unitVar"
    ctrlOut <- generateBindingWith "ctrl"
    let actuals' = [ctrlIn] ++ actuals ++ [unitVar]
    let e' = replaceUnitWithVar unitVar e
    -- construction of the `ctrl` call
    ie <- mkDestructured formals ctrlOut e'
    return $
        Lambda originalFormals $
        Let
            (Direct ctrlVar)
            (fromListToApply (Sf "ohua.lang/ctrl" Nothing) $ map (Var . Local) actuals')
            ie
    where
      replaceUnitWithVar unitVar (Lambda v e) = Lambda v $ replaceUnitWithVar unitVar e
      replaceUnitWithVar unitVar (Let v e ie) = Let v (replaceUnitWithVar unitVar e) (replaceUnitWithVar unitVar ir)
      replaceUnitWithVar unitVar (Apply e1 e2) = Apply (replaceUnitWithVar unitVar e1) (replaceUnitWithVar unitVar e2)
      replaceUnitWithVar unitVar someunitExpr = Var $ Local unitVar
      replaceUnitWithVar unitVar v@(Var _) = v
