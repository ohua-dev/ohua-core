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
ctrl :: Int -> [Vars] -> [[Vars]]
@

where the length of the outer list of the result is specified by the first parameter.
As such, `ctrl` does not return a `Bool` anymore. It is essentially the combination of the
former `ctrl` and `scope`.

For the case of smap, the new `ctrl` resembles the very same functionality that the
`oneToN` encapsulated.
For the case of conditionals, we need another function that converts a `Bool` to an `Int`:

@
boolToInt True  = 1
boolToInt False = 0
@

As such, we can write the following transformation (for one of the branches):

@
let cond :: Bool = ...
  let tBranchCtrl = boolToInt cond in
    let resultTrue = (\() -> let x = ctrl tBranchCtrl a b c in
                              ...
                               let y = independentFn x2 -- FIXME how do we deal with those?
                                ... true branch expression ... ) in ...
@

If `tBranchCtrl` is `0` then the branch is not executed, i.e., the `ctrl` function will
drop its inputs and not produce any output.

-}
module Ohua.ALang.Passes.Control where


import           Ohua.DFLang.Lang
import           Ohua.DFLang.Util
import qualified Ohua.DFLang.Refs as Refs

import qualified Data.HashMap as HM

transformCtrl :: DFExpr -> DFExpr
transformCtrl (DFExpr lets retVar) =
  let ctrlExprs = filter ((== Refs.ctrl) . functionRef) lets
      updated = flip map ctrlExprs $ \e ->
                  let bnd = case returnAssignment e of
                            Direct b -> b
                            otherwise -> error "Invariant broken: ctrl only has a single output!"
                      usageExprs = findUsages b ctrlExprs
                    in flip map usageExprs $ \ue ->
                          let inputs = callArguments ue
                              inputs' = flip delete inputs $ DFVar bnd
                           in ue@{callArguments=inputs', contextArg=Just bnd}
      updatedIds = HM.fromList $ zip (map callSiteId updated) updated
      lets' = foldl
                (\l e ->
                  let cId = callSiteId e
                       ne = case HS.lookup cId updatedIds of
                              Just updateExpr -> updatedExpr
                              Nothing -> e
                   in l ++ [ne])
                []
                lets
   in $ DFExpr lets' retVar
