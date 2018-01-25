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

import           Control.Monad.Except
import           Lens.Micro
import           Ohua.Internal.Logging
import           Ohua.Internal.Monad
import           Ohua.Types


-- | Alias for backwards compatibility with old `MonadOhua` interface
failWith :: MonadError Error m => Error -> m a
failWith = throwError


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = (^. l) <$> getEnvironment
