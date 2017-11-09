-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds       #-}
module Ohua.Internal.Monad where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.RWS.Strict hiding (fail)
import           Control.Monad.Writer
import qualified Data.HashSet             as HS
import           Data.List                (intercalate)
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Vector              as V
import           Lens.Micro.Platform
import           Ohua.ALang.Lang
import           Ohua.LensClasses
import           Ohua.Types as Ty
import           Ohua.Util


-- The compiler monad.
-- Encapsulates the state necessary to generate bindings
-- Allows IO actions.
-- In development this collects errors via a MonadWriter, in production this collection will
-- be turned off and be replaced by an exception, as such error should technically not occur
-- there
newtype OhuaM env a = OhuaM { runOhuaM :: RWST Environment Warnings (Ty.State env) (ExceptT Error IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)

class MonadGenBnd m where
    generateBinding :: m Binding
    generateBindingWith :: Binding -> m Binding

instance MonadGenBnd (OhuaM env) where
    generateBinding = OhuaM $ do
        taken <- use $ nameGenerator . takenNames
        (h:t) <- dropWhile (`HS.member` taken) <$> use (nameGenerator . simpleNameList)
        nameGenerator . simpleNameList .= t
        nameGenerator . takenNames %= HS.insert h
        pure h
    generateBindingWith (Binding prefix) = OhuaM $ do
        taken <- use $ nameGenerator . takenNames
        let (h:_) = dropWhile (`HS.member` taken) $ map (Binding . (prefix' <>) . T.pack . show) ([0..] :: [Int])
        nameGenerator . takenNames %= HS.insert h
        pure h
      where prefix' = prefix <> "_"

instance (MonadGenBnd m, Monad m) => MonadGenBnd (ReaderT e m) where
    generateBinding = lift generateBinding
    generateBindingWith = lift . generateBindingWith

instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (WriterT w m) where
    generateBinding = lift generateBinding
    generateBindingWith = lift . generateBindingWith

instance (MonadGenBnd m, Monad m) => MonadGenBnd (StateT s m) where
    generateBinding = lift generateBinding
    generateBindingWith = lift . generateBindingWith

instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (RWST e w s m) where
    generateBinding = lift generateBinding
    generateBindingWith = lift . generateBindingWith

class MonadGenId m where
    generateId :: m FnId

instance MonadGenId (OhuaM env) where
    generateId = OhuaM $ do
        idCounter %= succ
        FnId <$> use idCounter

instance (MonadGenId m, Monad m) => MonadGenId (ReaderT e m) where
    generateId = lift generateId
instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (WriterT w m) where
    generateId = lift generateId
instance (MonadGenId m, Monad m) => MonadGenId (StateT s m) where
    generateId = lift generateId
instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (RWST e w s m) where
    generateId = lift generateId

class MonadReadEnvExpr m where
    type EnvExpr m
    lookupEnvExpr :: HostExpr -> m (Maybe (EnvExpr m))

instance MonadReadEnvExpr (OhuaM env) where
    type EnvExpr (OhuaM env) = env
    lookupEnvExpr (HostExpr i) = OhuaM $ preuse (envExpressions . ix i)

instance (MonadReadEnvExpr m, Monad m) => MonadReadEnvExpr (ReaderT e m) where
    type EnvExpr (ReaderT e m) = EnvExpr m
    lookupEnvExpr = lift . lookupEnvExpr

instance (MonadReadEnvExpr m, Monad m, Monoid w) => MonadReadEnvExpr (WriterT w m) where
    type EnvExpr (WriterT e m) = EnvExpr m
    lookupEnvExpr = lift . lookupEnvExpr

instance (MonadReadEnvExpr m, Monad m) => MonadReadEnvExpr (StateT s m) where
    type EnvExpr (StateT s m) = EnvExpr m
    lookupEnvExpr = lift . lookupEnvExpr

instance (MonadReadEnvExpr m, Monad m, Monoid w) => MonadReadEnvExpr (RWST e w s m) where
    type EnvExpr (RWST e w s m) = EnvExpr m
    lookupEnvExpr = lift . lookupEnvExpr

class MonadRecordWarning m where
    recordWarning :: Warning -> m ()

instance MonadRecordWarning (OhuaM env) where
    recordWarning warn = OhuaM $ tell [warn]

instance (MonadRecordWarning m, Monad m) => MonadRecordWarning (ReaderT e m) where
    recordWarning = lift . recordWarning

instance (MonadRecordWarning m, Monad m, Monoid w) => MonadRecordWarning (WriterT w m) where
    recordWarning = lift . recordWarning

instance (MonadRecordWarning m, Monad m) => MonadRecordWarning (StateT s m) where
    recordWarning = lift . recordWarning

instance (MonadRecordWarning m, Monad m, Monoid w) => MonadRecordWarning (RWST e w s m) where
    recordWarning = lift . recordWarning

getEnvExpr :: (MonadError Error m, MonadReadEnvExpr m) => HostExpr -> m (EnvExpr m)
getEnvExpr =  maybe (throwError msg) pure <=< lookupEnvExpr
  where msg = "Invariant violated, host expression was not defined."

class MonadRecordEnvExpr m where
    type RecEnvExpr m
    addEnvExpression :: RecEnvExpr m -> m HostExpr

instance MonadRecordEnvExpr (OhuaM env) where
    type RecEnvExpr (OhuaM env) = env
    addEnvExpression expr = OhuaM $ do
        envExpressions %= (`V.snoc` expr)
        HostExpr . V.length <$> use envExpressions


instance (MonadRecordEnvExpr m, Monad m) => MonadRecordEnvExpr (ReaderT e m) where
    type RecEnvExpr (ReaderT e m) = RecEnvExpr m
    addEnvExpression = lift . addEnvExpression

instance (MonadRecordEnvExpr m, Monad m, Monoid w) => MonadRecordEnvExpr (WriterT w m) where
    type RecEnvExpr (WriterT w m) = RecEnvExpr m
    addEnvExpression = lift . addEnvExpression

instance (MonadRecordEnvExpr m, Monad m) => MonadRecordEnvExpr (StateT s m) where
    type RecEnvExpr (StateT s m) = RecEnvExpr m
    addEnvExpression = lift . addEnvExpression

instance (MonadRecordEnvExpr m, Monad m, Monoid w) => MonadRecordEnvExpr (RWST e w s m) where
    type RecEnvExpr (RWST e w s m) = RecEnvExpr m
    addEnvExpression = lift . addEnvExpression

class MonadReadEnvironment m where
    getEnvironment :: m Environment

instance MonadReadEnvironment (OhuaM env) where
    getEnvironment = OhuaM ask

instance (MonadReadEnvironment m, Monad m) => MonadReadEnvironment (ReaderT e m) where
    getEnvironment = lift getEnvironment

instance (MonadReadEnvironment m, Monad m) => MonadReadEnvironment (StateT s m) where
    getEnvironment = lift getEnvironment

instance (MonadReadEnvironment m, Monad m, Monoid w) => MonadReadEnvironment (WriterT w m) where
    getEnvironment = lift getEnvironment

instance (MonadReadEnvironment m, Monad m, Monoid w) => MonadReadEnvironment (RWST e w s m) where
    getEnvironment = lift getEnvironment

type MonadOhua env m = (MonadGenId m, MonadGenBnd m, MonadReadEnvExpr m, EnvExpr m ~ env, MonadRecordEnvExpr m, RecEnvExpr m ~ env, MonadError Error m, MonadIO m, MonadReadEnvironment m)

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runFromExpr :: Options -> (Expression -> OhuaM env result) -> Expression -> IO (Either Error (result, Warnings))
runFromExpr opts f tree = runFromBindings opts (f tree) $ HS.fromList $ extractBindings tree

runFromBindings :: Options -> OhuaM env result -> HS.HashSet Binding -> IO (Either Error (result, Warnings))
runFromBindings opts f taken = runExceptT $ evalRWST (runOhuaM f) env state
  where
    nameGen = initNameGen taken
    state = State nameGen 0 mempty
    env = Environment opts


runPrintWarns :: Options -> OhuaM env result -> HS.HashSet Binding -> IO (Either Error result)
runPrintWarns opts f taken =  runFromBindings opts f taken >>= \case
    Left err -> return $ Left err
    Right (val, errors) -> do
        unless (null errors) $ T.putStrLn $ T.intercalate "\n" errors
        return $ Right val

initNameGen :: HS.HashSet Binding -> NameGenerator
initNameGen taken = NameGenerator taken
    [ Binding $ T.pack $ char : maybe [] show num
    | num <- Nothing : map Just [(0 :: Integer)..]
    , char <- ['a'..'z']
    ]
