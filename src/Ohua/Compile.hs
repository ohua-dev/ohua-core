-- |
-- Module      : $Header$
-- Description : The compiler pipeline
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}
module Ohua.Compile where


import           Control.Monad.Except
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as HM
import           Debug.Trace
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.ALang.Optimizations
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.ALang.Show
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Optimizations
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types
import Ohua.Util
import Data.Monoid ((<>))


-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: MonadOhua m => Expression -> m OutGraph
pipeline e = do
    ssaE <- performSSA e
    normalizedE <- normalize ssaE

#ifdef DEBUG
    checkProgramValidity normalizedE
    checkHigherOrderFunctionSupport normalizedE
    Ohua.ALang.Passes.SSA.checkSSA normalizedE
#endif

    optimizedE <- Ohua.ALang.Optimizations.runOptimizations normalizedE

#ifdef DEBUG
    Ohua.ALang.Passes.SSA.checkSSA optimizedE
#endif

    traceShow optimizedE (return ())

    dfE <- lowerALang optimizedE

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr dfE
#endif

    optimizedDfE <- Ohua.DFLang.Optimizations.runOptimizations dfE

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr optimizedDfE
#endif
    trace (showDFExpr optimizedDfE) (return ())
    return $ toGraph optimizedDfE


-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: MonadError Error m => Expression -> m OutGraph
compile e = either throwError (return . fst) =<< flip runOhuaT e pipeline


-- | Verify that only higher order fucntions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $ failWith $ "Lambdas may only be input to higher order functions, not " <> showT f
        return True
    checkNestedExpr (Var (Sf n _)) = return $ HM.member n hofNames
    checkNestedExpr (Var _) = return False
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> showT a
    isLambda (Lambda _ _) = True
    isLambda _            = False
checkHigherOrderFunctionSupport (Var _) = return ()
checkHigherOrderFunctionSupport a = failWith $ "Expected let or var, got " <> showT a
