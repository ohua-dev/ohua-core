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
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.ALang.Optimizations
import           Ohua.ALang.Passes
import           Ohua.ALang.Passes.SSA
import           Ohua.DFGraph
import           Ohua.DFLang.Optimizations
import           Ohua.DFLang.Passes
import           Ohua.Monad
import           Ohua.Types


pipeline :: (MonadError String m, MonadOhua m) => Expression -> m OutGraph
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

    dfE <- lowerALang optimizedE

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr dfE
#endif

    optimizedDfE <- Ohua.DFLang.Optimizations.runOptimizations dfE

#ifdef DEBUG
    Ohua.DFLang.Passes.checkSSAExpr optimizedDfE
#endif
    -- Comment: I use `<&>` (aka `fmap`) here because `toGraph` does not run in a monad
    return $ toGraph optimizedDfE


compile :: MonadError String m => Expression -> m OutGraph
compile e = flip runOhuaT e pipeline


type SimplePass = Pass (OhuaT (Either String))


checkHigherOrderFunctionSupport :: MonadError String m => Expression -> m ()
checkHigherOrderFunctionSupport (Let _ e rest) = do
    checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $ throwError "Lambdas may only be input to higher order functions!"
        return True
    checkNestedExpr (Var (Sf n _)) = return $ HM.member n hofNames
    checkNestedExpr (Var _) = return False
    checkNestedExpr _ = throwError "Expected var or apply expr"
    isLambda (Lambda _ _) = True
    isLambda _            = False
checkHigherOrderFunctionSupport (Var _) = return ()
checkHigherOrderFunctionSupport _ = throwError "Expected let or var"
