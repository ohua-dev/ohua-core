{-# LANGUAGE Rank2Types #-}
module Ohua.Monad
  ( Ohua, OhuaEffList, Eff, Member, Members, runFromExpr, runFromBindings
    , GenId, generateId, resetIdCounter
    , GenBnd, generateBinding, generateBindingWith
    , ReadEnvExpr, lookupEnvExpr, getEnvExpr
    , RecordEnvExpr, addEnvExpression
    , Reader, ask
    , E.Error, OhuaErr, throwError, catchError, failWith
    , Logger, runStderrLogger, runHandleLogger, runSilentLogger, runLogger, filterLogger
    , LogLevel(..), LogSource, logDebugN, logInfoN
    , logWarnN, logErrorN, logOtherN
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error  as E
import           Control.Monad.Freer.Reader
import           Lens.Micro
import           Ohua.Internal.Logging
import           Ohua.Internal.Monad
import           Ohua.Types                 as Ty


-- | Alias for backwards compatibility with old `MonadOhua` interface
failWith :: Member (E.Error Ty.Error) effs => Ty.Error -> Eff effs a
failWith = throwError


fromEnv :: Member (Reader Environment) effs => Lens' Environment a -> Eff effs a
fromEnv l = (^. l) <$> ask
