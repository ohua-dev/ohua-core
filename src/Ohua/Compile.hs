-- |
-- Module      : $Header$
-- Description : The compiler pipeline
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Ohua.Compile where


import           Control.Monad.Except
import           Data.Default
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as HM
import           Data.Monoid               ((<>))
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
import           Ohua.Util


data CustomPasses m = CustomPasses
    { passAfterDFLowering :: DFExpr -> m DFExpr
    }

noCustomPasses :: Applicative m => CustomPasses m
noCustomPasses = CustomPasses pure

instance Applicative m => Default (CustomPasses m) where
    def = noCustomPasses





-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: MonadOhua envExpr m => CustomPasses m -> Expression -> m OutGraph
pipeline CustomPasses{..} e = do
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

    dfE <- lowerALang optimizedE

    forceTraceReport (showDFExpr dfE) ()

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr dfE
#endif

    dfAfterCustom <- passAfterDFLowering dfE

    optimizedDfE <- Ohua.DFLang.Optimizations.runOptimizations dfAfterCustom

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr optimizedDfE
#endif
    pure $ toGraph optimizedDfE




-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: MonadError Error m => CustomPasses (OhuaT env m) -> Expression -> m OutGraph
compile passes = either throwError (return . fst) <=< runOhuaT (pipeline passes)


-- | Verify that only higher order fucntions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua envExpr m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $ failWith $ "Lambdas may only be input to higher order functions, not " <> showT f
        pure True
    checkNestedExpr (Var (Sf n _)) = pure $ HM.member n hofNames
    checkNestedExpr (Var _) = pure False
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> showT a
    isLambda (Lambda _ _) = True
    isLambda _            = False
checkHigherOrderFunctionSupport (Var _) = pure ()
checkHigherOrderFunctionSupport a = failWith $ "Expected let or var, got " <> showT a
