-- |
-- Module      : $Header$
-- Description : Show algorithm language terms as lambda terms
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.ALang.Show where

import           Ohua.ALang.Lang
import           Ohua.Types


showAssigment :: Assignment -> String
showAssigment (Direct x)      = show x
showAssigment (Destructure d) = show d

showLambda :: Expression -> String
showLambda (Var x) = showSymbol x
showLambda (Lambda arg body) = "λ " ++ showAssigment arg ++ ". " ++ showLambda body ++ ""
showLambda (Let assignment value body) = "let " ++ showAssigment assignment ++ " = " ++ showLambda value ++ " in " ++ showLambda body
showLambda (Apply function argument) = showFunction function ++ " " ++ showArgument argument

showFunction :: Expression -> String
showFunction e@(Lambda _ _) = "(" ++ showLambda e ++ ")"
showFunction e@(Let _ _ _)  = "(" ++ showLambda e ++ ")"
showFunction e              = showLambda e

showArgument :: Expression -> String
showArgument e@(Lambda _ _) = "(" ++ showLambda e ++ ")"
showArgument e@(Let _ _ _)  = "(" ++ showLambda e ++ ")"
showArgument e@(Apply _ _)  = "(" ++ showLambda e ++ ")"
showArgument e              = showLambda e

showSymbol :: ResolvedSymbol -> String
showSymbol (Local l) = show l
showSymbol (Sf s i)   = "Sf[" ++ show s ++ "]" ++ maybe "" (\(FnId a) -> "<" ++ show a ++ ">") i
showSymbol (Algo a)  = "Algo[" ++ show a ++ "]"
showSymbol (Env e)   = show e
