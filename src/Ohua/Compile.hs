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

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM

import Ohua.ALang.Lang
import Ohua.ALang.Optimizations
import Ohua.ALang.Passes
import Ohua.ALang.Passes.SSA
import Ohua.ALang.Passes.TailRec (loadTailRecPasses)
import Ohua.DFGraph
import Ohua.DFLang.Optimizations
import Ohua.DFLang.PPrint ()
import Ohua.DFLang.Passes
import qualified Ohua.DFLang.Verify
import Ohua.Configuration
import Ohua.Stage


data CustomPasses env = CustomPasses
  { passAfterDFLowering :: DFExpr -> OhuaM env DFExpr
  , passAfterNormalize  :: Expression -> OhuaM env Expression
  }

noCustomPasses :: CustomPasses env
noCustomPasses = CustomPasses pure pure

instance Default (CustomPasses env) where
    def = noCustomPasses


forceLog :: (MonadLogger m, NFData a) => Text -> a -> m ()
forceLog msg a = a `deepseq` logDebugN msg


-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: CustomPasses env -> Expression -> OhuaM env OutGraph
pipeline CustomPasses {..} e = do
    stage resolvedAlang e

    ssaE <- performSSA e
    stage ssaAlang ssaE

    normalizedE <- normalize =<< passBeforeNormalize ssaE
    stage normalizedAlang normalizedE

    whenDebug $ do
        checkProgramValidity normalizedE
        checkHigherOrderFunctionSupport normalizedE
        Ohua.ALang.Passes.SSA.checkSSA normalizedE

    customAfterNorm <- passAfterNormalize normalizedE
    stage customAlangPasses customAfterNorm

    optimizedE <-
        Ohua.ALang.Optimizations.runOptimizations =<< normalize customAfterNorm
    stage optimizedAlang optimizedE

    whenDebug $ Ohua.ALang.Passes.SSA.checkSSA optimizedE
    dfE <- lowerALang =<< normalize optimizedE
    stage initialDflang dfE

    Ohua.DFLang.Verify.verify dfE
    whenDebug $ Ohua.DFLang.Passes.checkSSAExpr dfE
    dfAfterCustom <- passAfterDFLowering dfE
    stage customDflang dfAfterCustom

    optimizedDfE <- Ohua.DFLang.Optimizations.runOptimizations dfAfterCustom
    stage optimizedDflang optimizedE

    whenDebug $ Ohua.DFLang.Passes.checkSSAExpr optimizedDfE
    pure $ toGraph optimizedDfE


-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: (MonadError Error m, MonadLoggerIO m) => Options -> CustomPasses env -> Expression -> m OutGraph
compile opts passes exprs = do
    logFn <- askLoggerIO
    let passes' = flip loadTailRecPasses passes $ view enableRecursiveFunctions opts
    either throwError pure =<< liftIO (runLoggingT (runFromExpr opts (pipeline passes') exprs) logFn)


-- | Verify that only higher order fucntions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua envExpr m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    void $ checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $
            failWith $
            "Lambdas may only be input to higher order functions, not " <>
            show f
        pure True
    checkNestedExpr (Var (Sf n _)) = pure $ HM.member n hofNames
    checkNestedExpr (Var _) = pure False
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> show a
    isLambda (Lambda _ _) = True
    isLambda _ = False
checkHigherOrderFunctionSupport (Var _) = pure ()
checkHigherOrderFunctionSupport a = failWith $ "Expected let or var, got " <> show a
