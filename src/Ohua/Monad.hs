{-# LANGUAGE Rank2Types #-}
module Ohua.Monad
    ( OhuaM, runFromExpr, runFromBindings, runPrintWarns
    , MonadGenId(generateId)
    , MonadGenBnd(generateBinding, generateBindingWith)
    , MonadReadEnvExpr(lookupEnvExpr), getEnvExpr
    , MonadRecordEnvExpr(addEnvExpression)
    , MonadRecordWarning(recordWarning)
    , MonadReadEnvironment(getEnvironment), fromEnv
    , MonadIO(liftIO)
    , MonadError(throwError, catchError), failWith
    , MonadOhua
    ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Lens.Micro
import           Ohua.Internal.Monad
import           Ohua.Types


failWith :: MonadError Error m => Error -> m a
failWith = throwError


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = (^. l) <$> getEnvironment
