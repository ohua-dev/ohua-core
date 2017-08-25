-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Ohua.Monad
    ( OhuaT, runOhuaT, runOhuaT0
    , MonadOhua(modifyState, getState, recordError)
    , generateBinding, generateBindingWith, generateId
    , MonadIO(..)
    ) where


import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.HashSet             as HS
import           Data.List                (intercalate)
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.LensClasses
import           Ohua.Types
import Data.Monoid
import qualified Data.Text as T


type Error = String

data CompilerState = CompilerState
    { compilerStateNameGenerator :: !NameGenerator
    , compilerStateIdCounter     :: !Int
    }
data CompilerEnv

instance HasNameGenerator CompilerState NameGenerator where
    nameGenerator = lens compilerStateNameGenerator $ \s a -> s { compilerStateNameGenerator = a }

instance HasIdCounter CompilerState Int where
    idCounter = lens compilerStateIdCounter $ \s a -> s { compilerStateIdCounter = a }

data NameGenerator = NameGenerator
    { nameGeneratorTakenNames     :: !(HS.HashSet Binding)
    , nameGeneratorSimpleNameList :: [Binding]
    }

instance HasTakenNames NameGenerator (HS.HashSet Binding) where
    takenNames = lens nameGeneratorTakenNames $ \s a -> s { nameGeneratorTakenNames = a }
instance HasSimpleNameList NameGenerator [Binding] where
    simpleNameList = lens nameGeneratorSimpleNameList $ \s a -> s { nameGeneratorSimpleNameList = a }

-- Only collect errors in debug mode
#ifdef DEBUG
type Errors = [Error]
#else
type Errors = ()
#endif

-- The compiler monad.
-- Encapsulates the state necessary to generate bindings
-- Allows IO actions.
-- In development this collects errors via a MonadWriter, in production this collection will
-- be turned off and be replaced by an exception, as such error should technically not occur
-- there
newtype OhuaT m a = OhuaT { runOhuaT' :: RWST CompilerEnv Errors CompilerState m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- Convenience typeclass.
-- This class is intend to make it simpler to use functionality related to the ohua monad
-- from an arbitrary monad
--
-- The typical use case is that you want to implement a compiler pass but you need additional
-- state or environment (see 'SSAM')
-- You achieve this by wrapping 'OhuaC' in a transformer ('StateT' for instance).
-- By implementing 'MonadOhua' (usually the implementation will be @lift@ or @lift .
-- unwrapMyCustomMonad@) you can use compiler functionality such as 'generateBinding'
-- from your custom monad without using @lift@.
class Monad m => MonadOhua m where
    recordError :: Error -> m ()
    modifyState :: (CompilerState -> CompilerState) -> m ()
    getState :: m CompilerState

instance Monad m => MonadOhua (OhuaT m) where
#ifdef DEBUG
    -- If we are in debug mode then record the error and forge on
    recordError err = OhuaT $ tell [err]
#else
    -- If we are in production just throw an exception
    recordError = error
#endif
    modifyState = OhuaT . modify
    getState = OhuaT get

-- A bit of magic to make every `MonadTrans` instance also a `MonadOhua` instance
instance {-# OVERLAPPABLE #-} (MonadOhua m, MonadTrans t, Monad (t m)) => MonadOhua (t m) where
    recordError = lift . recordError
    modifyState = lift . modifyState
    getState = lift getState

instance (MonadError e m) => MonadError e (OhuaT m) where
    throwError = OhuaT . throwError
    catchError (OhuaT m) f = OhuaT $ m `catchError` (runOhuaT' . f)


fromState :: MonadOhua m => Lens' CompilerState a -> m a
fromState l = (^. l) <$> getState

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runOhuaT :: Monad ctxt => (Expression -> OhuaT ctxt result) -> Expression -> ctxt result
runOhuaT f tree = runOhuaT0 (f tree) $ HS.fromList $ extractBindings tree

runOhuaT0 :: Monad ctxt => OhuaT ctxt result -> HS.HashSet Binding -> ctxt result
runOhuaT0 f taken = do
    (val, errors) <- evalRWST (runOhuaT' f) (error "Ohua has no environment!") (CompilerState nameGen 0)
#ifdef DEBUG
    unless (null errors) $ error $ intercalate "\n" errors
#endif
    return val
  where
    nameGen = initNameGen taken

initNameGen :: HS.HashSet Binding -> NameGenerator
initNameGen taken = NameGenerator taken
    [ Binding $ T.pack $ char : maybe [] show num
    | num <- Nothing : map Just [(0 :: Integer)..]
    , char <- ['a'..'z']
    ]

generateBinding :: MonadOhua m => m Binding
generateBinding = do
    taken <- fromState $ nameGenerator . takenNames
    (h:t) <- dropWhile (`HS.member` taken) <$> fromState (nameGenerator . simpleNameList)
    modifyState $ nameGenerator . simpleNameList .~ t
    modifyState $ nameGenerator . takenNames %~ HS.insert h
    return h

generateBindingWith :: MonadOhua m => Binding -> m Binding
generateBindingWith (Binding prefix) = do
    taken <- fromState $ nameGenerator . takenNames
    let (h:_) = dropWhile (`HS.member` taken) $ map (Binding . (prefix' <>) . T.pack . show) ([0..] :: [Int])
    modifyState $ nameGenerator . takenNames %~ HS.insert h
    return h
  where prefix' = prefix <> "_"

generateId :: MonadOhua m => m FnId
generateId = do
    modifyState $ idCounter %~ succ
    FnId <$> fromState idCounter
