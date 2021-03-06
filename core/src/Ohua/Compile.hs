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
{-# LANGUAGE RecordWildCards #-}

module Ohua.Compile where

import Ohua.Prelude

import qualified Data.HashSet as HS

import Control.Lens (view)
import Ohua.ALang.Lang
import Ohua.ALang.Passes
import Ohua.ALang.Passes.SSA
import Ohua.Feature.TailRec (loadTailRecPasses)
import Ohua.ALang.Passes.Verify
import Ohua.ALang.Refs as Refs
import Ohua.Compile.Configuration
import Ohua.DFGraph
import Ohua.DFLang.PPrint ()
import Ohua.DFLang.Passes
import qualified Ohua.DFLang.Verify
import Ohua.Stage
import Ohua.Feature.TailRec.Passes.ALang (y)

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
    coreE <- Ohua.ALang.Passes.runCorePasses =<< normalize customAfterNorm
    stage coreAlang coreE
    whenDebug $ do
        Ohua.ALang.Passes.SSA.checkSSA coreE
        Ohua.ALang.Passes.Verify.checkInvariants coreE
    dfE <- lowerALang =<< normalize coreE
    stage initialDflang dfE
    Ohua.DFLang.Verify.verify dfE
    whenDebug $ Ohua.DFLang.Passes.checkSSAExpr dfE
    dfAfterCustom <- passAfterDFLowering dfE
    stage customDflang dfAfterCustom
    coreDfE <- Ohua.DFLang.Passes.runCorePasses dfAfterCustom
    stage coreDflang coreDfE
    whenDebug $ Ohua.DFLang.Passes.checkSSAExpr coreDfE
    pure $ toGraph coreDfE

-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile ::
       (MonadError Error m, MonadLoggerIO m)
    => Options
    -> CustomPasses env
    -> Expression
    -> m OutGraph
compile opts passes exprs = do
    logFn <- askLoggerIO
    let passes' =
            flip loadTailRecPasses passes $
            view transformRecursiveFunctions opts
    either throwError pure =<<
        liftIO (runLoggingT (runFromExpr opts (pipeline passes') exprs) logFn)

stdHofNames :: HashSet QualifiedBinding
stdHofNames = HS.fromList [Refs.smap, Refs.ifThenElse, Refs.seq, Refs.recur, y]

-- | Verify that only higher order functions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua m => Expression -> m ()
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
    checkNestedExpr (PureFunction n _)
        | HS.member n stdHofNames = pure True
        | otherwise = HS.member n . (view $ options . higherOrderFunctions) <$> getEnvironment
    checkNestedExpr (Var _) = pure False
    checkNestedExpr (BindState _ m) = checkNestedExpr m
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> show a
    isLambda (Lambda _ _) = True
    isLambda _ = False
checkHigherOrderFunctionSupport (Var _) = pure ()
checkHigherOrderFunctionSupport a =
    failWith $ "Expected let or var, got " <> show a
