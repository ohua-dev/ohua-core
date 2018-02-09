-- |
-- Module      : $Header$
-- Description : The compiler pipeline
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module Ohua.Compile where


import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Freer
import           Data.Default
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as HM
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
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
import qualified Ohua.Util.Str             as Str

import           Control.Monad.Freer.State
import qualified Data.Vector               as V


data CustomPasses env = CustomPasses
  { passAfterDFLowering :: forall effs . (Ohua effs, Members '[RecordEnvExpr env, ReadEnvExpr env] effs ) => DFExpr -> Eff effs DFExpr
  }

noCustomPasses :: CustomPasses env
noCustomPasses = CustomPasses pure

instance Default (CustomPasses env) where
  def = noCustomPasses


forceLog :: (Member Logger effs, NFData a) => Text -> a -> Eff effs ()
forceLog msg a = a `deepseq` logDebugN msg


-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: (Ohua effs, Members '[ReadEnvExpr env, RecordEnvExpr env] effs) => CustomPasses env -> Expression -> Eff effs OutGraph
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

    dfAfterCustom <-  passAfterDFLowering dfE

    optimizedDfE <- Ohua.DFLang.Optimizations.runOptimizations dfAfterCustom

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr optimizedDfE
#endif
    pure $ toGraph optimizedDfE




-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: forall effs env . (Members '[OhuaErr, IO, Logger] effs) => Options -> CustomPasses env -> Expression -> Eff effs OutGraph
compile opts passes = runFromExpr @effs @env opts (pipeline passes)


-- | Verify that only higher order fucntions have lambdas as arguments
checkHigherOrderFunctionSupport :: Ohua effs => Expression -> Eff effs ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    checkNestedExpr e
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
