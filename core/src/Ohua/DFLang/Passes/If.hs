module Ohua.DFLang.Passes.If where

import Ohua.Prelude

import qualified Data.List.NonEmpty as NE (fromList)
import Data.Sequence as DS ((><), fromList)

import Ohua.DFLang.Lang
import qualified Ohua.DFLang.Refs as Refs
import Ohua.DFLang.Util (findAllExprs, findUsages, removeAllExprs)

-- | We turn "ifFun" into an operator such that we can get rid of the desturcuting.
optimizeIf :: DFExpr -> DFExpr
optimizeIf (DFExpr letExprs returnVar) =
    let ifFuns = toList $ findAllExprs Refs.ifFun letExprs
        (newIfFuns, orphanedNths) = unzip $ toList $ map updateIfFuns ifFuns
        orphanedNths' = catMaybes $ join orphanedNths
        letExprs' =
            removeAllExprs (DS.fromList $ ifFuns ++ orphanedNths') letExprs
     in flip DFExpr returnVar $ letExprs' >< DS.fromList newIfFuns
  where
    updateIfFuns ifFun@(LetExpr {output = outs, functionRef = DFFnRef _ qb}) =
        let (newOuts, nths) =
                Ohua.Prelude.unzip $
                flip map outs $ \out ->
                    let (nth:[]) = findUsages out letExprs
                        nthOuts = output nth
                     in if (length nthOuts) == 1
                            then (head $ NE.fromList nthOuts, Just nth)
                            else error "Invariant broken"
         in ( ifFun {output = newOuts, functionRef = DFFnRef OperatorNode qb}
            , nths)
