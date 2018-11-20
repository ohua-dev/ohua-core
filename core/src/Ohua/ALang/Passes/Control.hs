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
                    in flip map usageExprs \ue ->
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
