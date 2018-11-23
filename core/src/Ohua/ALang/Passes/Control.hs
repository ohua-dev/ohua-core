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

-}
module Ohua.ALang.Passes.Control where

import Ohua.Prelude

import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util
import Ohua.Types

import qualified Data.HashMap.Lazy as HM
import Data.List (delete)
import Data.Maybe
-- transformCtrl :: DFExpr -> DFExpr
-- transformCtrl (DFExpr lets retVar) =
--     let ctrlExprs = filter ((== Refs.ctrl) . functionRef) $ foldl concat [] lets
--         updated =
--             join $
--             flip map ctrlExprs $ \e ->
--                 let bnd =
--                         case returnAssignment e of
--                             Direct b -> b
--                             otherwise ->
--                                 error
--                                     "Invariant broken: ctrl only has a single output!"
--                     usageExprs = findUsages bnd ctrlExprs
--                  in flip map usageExprs $ \ue ->
--                         let inputs = callArguments ue
--                             inputs' = flip delete inputs $ DFVar bnd
--                          in ue {callArguments = inputs', contextArg = Just bnd}
--         updatedIds = HM.fromList $ zip (map callSiteId updated) updated
--         lets' =
--             DS.fromList $
--             foldl
--                 (\l e ->
--                      let cId = callSiteId e
--                       in let ne =
--                                  case HM.lookup cId updatedIds of
--                                      Just updateExpr -> updatedExpr
--                                      Nothing -> e
--                           in l ++ [ne])
--                 []
--                 lets
--      in DFExpr lets' retVar
