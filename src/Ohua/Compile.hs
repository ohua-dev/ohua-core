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


import           Control.DeepSeq
import           Control.Monad.Except
import           Data.Default.Class
import qualified Data.HashMap.Strict       as HM
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Ohua.ALang.Lang
import           Ohua.ALang.Optimizations
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.DFGraph
import           Ohua.DFLang.Lang
import           Ohua.DFLang.Optimizations
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types
import qualified Ohua.Util.Str             as Str


data CustomPasses env = CustomPasses
  { passAfterDFLowering :: DFExpr -> OhuaM env DFExpr
  }

noCustomPasses :: CustomPasses env
noCustomPasses = CustomPasses pure

instance Default (CustomPasses env) where
  def = noCustomPasses


forceLog :: (MonadLogger m, NFData a) => Text -> a -> m ()
forceLog msg a = a `deepseq` logDebugN msg


-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: CustomPasses env -> Expression -> OhuaM env OutGraph
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

    forceLog (pack $ Str.toString $ showDFExpr dfE) ()

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
compile :: (MonadError Error m, MonadLoggerIO m) => Options -> CustomPasses env -> Expression -> m OutGraph
compile opts passes exprs = do
    logFn <- askLoggerIO
    either throwError pure =<< liftIO (runLoggingT (runFromExpr opts (pipeline passes) exprs) logFn)


-- | Verify that only higher order fucntions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua envExpr m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    void $ checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $ failWith $ "Lambdas may only be input to higher order functions, not " <> Str.showS f
        pure True
    checkNestedExpr (Var (Sf n _)) = pure $ HM.member n hofNames
    checkNestedExpr (Var _) = pure False
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> Str.showS a
    isLambda (Lambda _ _) = True
    isLambda _            = False
checkHigherOrderFunctionSupport (Var _) = pure ()
checkHigherOrderFunctionSupport a = failWith $ "Expected let or var, got " <> Str.showS a
