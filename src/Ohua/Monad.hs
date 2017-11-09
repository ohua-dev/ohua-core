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

import Ohua.Internal.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Ohua.Types
import Lens.Micro


failWith :: MonadError Error m => Error -> m a
failWith = throwError


fromEnv :: (MonadReadEnvironment m, Functor m) => Lens' Environment a -> m a
fromEnv l = (^. l) <$> getEnvironment
