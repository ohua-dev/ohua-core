{-# LANGUAGE Rank2Types #-}
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
    , generateBindingIn, generateBindingWithIn
    , generateBindingFromGenerator, generateBindingFromGeneratorWith
    , initNameGen
    ) where

import Protolude

import Control.Monad.Logger
import System.Log.FastLogger (fromLogStr)
import Lens.Micro

import Ohua.Internal.Monad
import Ohua.Types

runSilentLoggingT :: LoggingT m a -> m a
runSilentLoggingT = flip runLoggingT $ \_ _ _ _ -> pure ()

-- | Alias for backwards compatibility with old `MonadOhua` interface
failWith :: MonadError Error m => Error -> m a
failWith = throwError


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = (^. l) <$> getEnvironment

runHandleLoggingT :: Handle -> LoggingT m a -> m a
runHandleLoggingT h = (`runLoggingT` output)
  where
    output loc src level msg = hPutStr h $ fromLogStr $ defaultLogStr loc src level msg
