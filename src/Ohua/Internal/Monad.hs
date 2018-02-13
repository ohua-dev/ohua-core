-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Ohua.Internal.Monad where


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy
import           Control.Monad.RWS.Strict   hiding (fail)
import qualified Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict
import           Control.Monad.Writer
import qualified Data.HashSet               as HS
import qualified Data.Vector                as V
import           Lens.Micro.Platform
import           Ohua.ALang.Lang
import           Ohua.Internal.Logging
import           Ohua.Types                 as Ty
import qualified Ohua.Util.Str              as Str


import           Control.DeepSeq

-- The compiler monad.
-- Encapsulates the state necessary to generate bindings
-- Allows IO actions.
-- In development this collects errors via a MonadWriter, in production this collection will
-- be turned off and be replaced by an exception, as such errors should technically not occur
-- there
newtype OhuaM env a = OhuaM
  { runOhuaM :: RWST Environment () (Ty.State env) (ExceptT Error (LoggingT IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError Error
             , MonadLogger
             , MonadLoggerIO
             )

class MonadGenBnd m where
    generateBinding :: m Binding
    default generateBinding :: (MonadGenBnd n, MonadTrans t, Monad n, t n ~ m) => m Binding
    generateBinding = lift generateBinding
    generateBindingWith :: Binding -> m Binding
    default generateBindingWith :: (MonadGenBnd n, MonadTrans t, Monad n, t n ~ m) => Binding -> m Binding
    generateBindingWith = lift . generateBindingWith

deepseqM :: (Monad m, NFData a) => a -> m ()
deepseqM a = a `deepseq` pure ()

instance MonadGenBnd (OhuaM env) where
    generateBinding = OhuaM $ generateBindingIn nameGenerator
    generateBindingWith = OhuaM . generateBindingWithIn nameGenerator

generateBindingFromGenerator :: NameGenerator -> (Binding, NameGenerator)
generateBindingFromGenerator g = (h, g')
  where
    taken = g ^. takenNames
    (h,t) = case dropWhile (`HS.member` taken) (g ^. simpleNameList) of
              (x:xs) -> (x, xs)
              [] -> error "Simple names is empty, this should be impossible"
    g' = g & simpleNameList .~ t
           & takenNames %~ HS.insert h

generateBindingFromGeneratorWith :: Binding -> NameGenerator -> (Binding, NameGenerator)
generateBindingFromGeneratorWith (Binding prefix) g = (h, g')
  where
    taken = g ^. takenNames
    prefix' = prefix <> "_"
    h = head $ dropWhile (`HS.member` taken) $ map (Binding . (prefix' <>) . Str.showS) ([0..] :: [Int])
    g' = g & takenNames %~ HS.insert h

generateBindingIn :: MonadState s m => Lens' s NameGenerator -> m Binding
generateBindingIn accessor = do
  (bnd, gen') <- generateBindingFromGenerator <$> use accessor
  accessor .= gen'
  pure bnd

generateBindingWithIn :: MonadState s m => Lens' s NameGenerator -> Binding -> m Binding
generateBindingWithIn accessor prefix = do
  (bnd, gen') <- generateBindingFromGeneratorWith prefix <$> use accessor
  accessor .= gen'
  pure bnd

instance (MonadGenBnd m, Monad m) => MonadGenBnd (ReaderT e m)
instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (WriterT w m)
instance (MonadGenBnd m, Monad m) => MonadGenBnd (Control.Monad.State.Strict.StateT s m)
instance (MonadGenBnd m, Monad m) => MonadGenBnd (Control.Monad.State.Lazy.StateT s m)
instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (Control.Monad.RWS.Strict.RWST e w s m)
instance (MonadGenBnd m, Monad m, Monoid w) => MonadGenBnd (Control.Monad.RWS.Lazy.RWST e w s m)

class MonadGenId m where
    generateId :: m FnId
    default generateId :: (MonadGenId n, Monad n, MonadTrans t, t n ~ m) => m FnId
    generateId = lift generateId
    -- | Unsafe. Only use this if yoy know what you are doing!!
    resetIdCounter :: FnId -> m ()
    default resetIdCounter :: (MonadGenId n, Monad n, MonadTrans t, t n ~ m) => FnId -> m ()
    resetIdCounter = lift . resetIdCounter


instance MonadGenId (OhuaM env) where
  generateId = OhuaM $ do
    idCounter %= succ
    use idCounter
  resetIdCounter val = OhuaM $ idCounter .= val

instance (MonadGenId m, Monad m) => MonadGenId (ReaderT e m)
instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (WriterT w m)
instance (MonadGenId m, Monad m) => MonadGenId (Control.Monad.State.Strict.StateT s m)
instance (MonadGenId m, Monad m) => MonadGenId (Control.Monad.State.Lazy.StateT s m)
instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (Control.Monad.RWS.Lazy.RWST e w s m)
instance (MonadGenId m, Monad m, Monoid w) => MonadGenId (Control.Monad.RWS.Strict.RWST e w s m)

class HasEnvExpr (m :: * -> *) where
    type EnvExpr m

instance HasEnvExpr (OhuaM e) where
    type EnvExpr (OhuaM e) = e

instance HasEnvExpr (ReaderT e m) where
    type EnvExpr (ReaderT e m) = EnvExpr m
instance HasEnvExpr (WriterT w m) where
    type EnvExpr (WriterT w m) = EnvExpr m
instance HasEnvExpr (Control.Monad.State.Strict.StateT s m) where
    type EnvExpr (Control.Monad.State.Strict.StateT s m) = EnvExpr m
instance HasEnvExpr (Control.Monad.State.Lazy.StateT s m) where
    type EnvExpr (Control.Monad.State.Lazy.StateT s m) = EnvExpr m
instance HasEnvExpr (Control.Monad.RWS.Lazy.RWST e w s m) where
    type EnvExpr (Control.Monad.RWS.Lazy.RWST e w s m) = EnvExpr m
instance HasEnvExpr (Control.Monad.RWS.Strict.RWST e w s m) where
    type EnvExpr (Control.Monad.RWS.Strict.RWST e w s m) = EnvExpr m

class HasEnvExpr m => MonadReadEnvExpr m where
    lookupEnvExpr :: HostExpr -> m (Maybe (EnvExpr m))
    default lookupEnvExpr :: (MonadReadEnvExpr n, MonadTrans t, Monad n, m ~ t n, EnvExpr m ~ EnvExpr n) => HostExpr -> m (Maybe (EnvExpr m))
    lookupEnvExpr = lift . lookupEnvExpr

instance MonadReadEnvExpr (OhuaM env) where
    lookupEnvExpr (HostExpr i) = OhuaM $ preuse (envExpressions . ix i)

instance (MonadReadEnvExpr m, Monad m) => MonadReadEnvExpr (ReaderT e m)
instance (MonadReadEnvExpr m, Monad m, Monoid w) => MonadReadEnvExpr (WriterT w m)
instance (MonadReadEnvExpr m, Monad m) => MonadReadEnvExpr (Control.Monad.State.Strict.StateT s m)
instance (MonadReadEnvExpr m, Monad m) => MonadReadEnvExpr (Control.Monad.State.Lazy.StateT s m)
instance (MonadReadEnvExpr m, Monad m, Monoid w) => MonadReadEnvExpr (Control.Monad.RWS.Lazy.RWST e w s m)
instance (MonadReadEnvExpr m, Monad m, Monoid w) => MonadReadEnvExpr (Control.Monad.RWS.Strict.RWST e w s m)

getEnvExpr :: (MonadError Error m, MonadReadEnvExpr m) => HostExpr -> m (EnvExpr m)
getEnvExpr =  maybe (throwError msg) pure <=< lookupEnvExpr
  where msg = "Invariant violated, host expression was not defined."

class HasEnvExpr m => MonadRecordEnvExpr m where
    addEnvExpression :: EnvExpr m -> m HostExpr
    default addEnvExpression :: (MonadTrans t, Monad n, MonadRecordEnvExpr n, t n ~ m, EnvExpr m ~ EnvExpr n) => EnvExpr m -> m HostExpr
    addEnvExpression = lift . addEnvExpression

instance MonadRecordEnvExpr (OhuaM env) where
    addEnvExpression expr = OhuaM $ do
        envExpressions %= (`V.snoc` expr)
        HostExpr . V.length <$> use envExpressions


instance (MonadRecordEnvExpr m, Monad m) => MonadRecordEnvExpr (ReaderT e m)
instance (MonadRecordEnvExpr m, Monad m, Monoid w) => MonadRecordEnvExpr (WriterT w m)
instance (MonadRecordEnvExpr m, Monad m) => MonadRecordEnvExpr (Control.Monad.State.Strict.StateT s m)
instance (MonadRecordEnvExpr m, Monad m) => MonadRecordEnvExpr (Control.Monad.State.Lazy.StateT s m)
instance (MonadRecordEnvExpr m, Monad m, Monoid w) => MonadRecordEnvExpr (Control.Monad.RWS.Strict.RWST e w s m)
instance (MonadRecordEnvExpr m, Monad m, Monoid w) => MonadRecordEnvExpr (Control.Monad.RWS.Lazy.RWST e w s m)

class MonadReadEnvironment m where
    getEnvironment :: m Environment
    default getEnvironment :: (MonadTrans t, Monad n, MonadReadEnvironment n, t n ~ m) => m Environment
    getEnvironment = lift getEnvironment

instance MonadReadEnvironment (OhuaM env) where
    getEnvironment = OhuaM ask

instance (MonadReadEnvironment m, Monad m) => MonadReadEnvironment (ReaderT e m)
instance (MonadReadEnvironment m, Monad m) => MonadReadEnvironment (Control.Monad.State.Lazy.StateT s m)
instance (MonadReadEnvironment m, Monad m) => MonadReadEnvironment (Control.Monad.State.Strict.StateT s m)
instance (MonadReadEnvironment m, Monad m, Monoid w) => MonadReadEnvironment (WriterT w m)
instance (MonadReadEnvironment m, Monad m, Monoid w) => MonadReadEnvironment (Control.Monad.RWS.Lazy.RWST e w s m)
instance (MonadReadEnvironment m, Monad m, Monoid w) => MonadReadEnvironment (Control.Monad.RWS.Strict.RWST e w s m)

type MonadOhua env m = ( MonadGenId m
                       , MonadGenBnd m
                       , MonadReadEnvExpr m
                       , MonadRecordEnvExpr m
                       , MonadError Error m
                       , MonadIO m
                       , MonadReadEnvironment m
                       , EnvExpr m ~ env
                       , MonadLogger m
                       )

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runFromExpr :: Options -> (Expression -> OhuaM env result) -> Expression -> LoggingT IO (Either Error result)
runFromExpr opts f tree = runFromBindings opts (f tree) $ HS.fromList $ extractBindings tree

runFromBindings :: Options -> OhuaM env result -> HS.HashSet Binding -> LoggingT IO (Either Error result)
runFromBindings opts f taken = runExceptT $ fst <$> evalRWST (runOhuaM f) env s0
  where
    nameGen = initNameGen taken
    s0 = State nameGen 0 mempty
    env = Environment opts


initNameGen :: HS.HashSet Binding -> NameGenerator
initNameGen taken = NameGenerator taken
    [ Binding $ Str.fromString $ char : maybe [] show num
    | num <- Nothing : map Just [(0 :: Integer)..]
    , char <- ['a'..'z']
    ]
