{-# LANGUAGE CPP #-}

#include "compat.h"

module Ohua.Monad
    ( OhuaM, runFromExpr, runFromBindings
    , MonadGenId(generateId, resetIdCounter)
    , MonadGenBnd(generateBinding, generateBindingWith)
    , HasEnvExpr(EnvExpr)
    , MonadReadEnvExpr(lookupEnvExpr), getEnvExpr
    , MonadRecordEnvExpr(addEnvExpression)
    , MonadReadEnvironment(getEnvironment), fromEnv
    , MonadIO(liftIO)
    , MonadError(throwError, catchError), failWith
    , MonadLogger, LoggingT, runStderrLoggingT, runHandleLoggingT, runSilentLoggingT, runLoggingT, filterLogger
    , MonadLoggerIO(askLoggerIO)
    , LogLevel(..), LogSource, logDebugN, logInfoN
    , logWarnN, logErrorN, logOtherN
    , MonadOhua
    -- ** Helper functions for building instances of 'MonadGenBnd'
    , GenBndT, runGenBndT
    , generateBindingIn, generateBindingWithIn
    , generateBindingFromGenerator, generateBindingFromGeneratorWith
    , initNameGen
    ) where

import Universum

import Control.Monad.Logger
import System.Log.FastLogger (fromLogStr)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Data.ByteString (hPutStr)

import Ohua.Internal.Monad
import Ohua.Types
import Ohua.Util

runSilentLoggingT :: LoggingT m a -> m a
runSilentLoggingT = flip runLoggingT $ \_ _ _ _ -> pure ()

-- | Alias for backwards compatibility with old `MonadOhua` interface
failWith :: (HasCallStack, MonadError Error m) => Error -> m a
failWith = throwErrorDebugS


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = view l <$> getEnvironment

runHandleLoggingT :: Handle -> LoggingT m a -> m a
runHandleLoggingT h = (`runLoggingT` output)
  where
    output loc src level msg = Universum.hPutStr h $ fromLogStr $ defaultLogStr loc src level msg
