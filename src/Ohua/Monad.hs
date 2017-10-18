-- |
-- Module      : $Header$
-- Description : The base Ohua compiler monad
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : POSIX

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Ohua.Monad
    ( OhuaT, runOhuaT, runOhuaT0, runOhuaT0IO
    , MonadOhua(onState, recordWarning, failWith)
    , generateBinding, generateBindingWith, generateId
    , MonadIO(..)
    , addEnvExpression
    ) where


import           Control.Monad.Except
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict hiding (fail)
import           Control.Monad.State
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
import           Ohua.Types
import           Ohua.Util


-- The compiler monad.
-- Encapsulates the state necessary to generate bindings
-- Allows IO actions.
-- In development this collects errors via a MonadWriter, in production this collection will
-- be turned off and be replaced by an exception, as such error should technically not occur
-- there
newtype OhuaT env m a = OhuaT { runOhuaT' :: RWST CompilerEnv Warnings (CompilerState env) (ExceptT Error m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (OhuaT env) where
    lift = OhuaT . lift . lift

-- Convenience typeclass.
-- This class is intend to make it simpler to use functionality related to the ohua monad
-- from an arbitrary monad
--
-- The typical use case is that you want to implement a compiler pass but you need additional
-- state or environment (see 'SSAM')
-- You achieve this by wrapping 'OhuaT' in a transformer ('StateT' for instance).
-- By implementing 'MonadOhua' (usually the implementation will be @lift@ or @lift .
-- unwrapMyCustomMonad@) you can use compiler functionality such as 'generateBinding'
-- from your custom monad without using @lift@.
class Monad m => MonadOhua envExpr m | m -> envExpr where
    -- | Record an error or warning but continue computation
    recordWarning :: Warning -> m ()
    -- | Failt the compiler with an error
    onState :: (CompilerState envExpr -> (a, CompilerState envExpr)) -> m a
    failWith :: Error -> m a

instance Monad m => MonadOhua env (OhuaT env m) where
    recordWarning err = OhuaT $ tell [err]
    onState = OhuaT . state
    failWith = OhuaT . throwError

-- A bit of magic to make every `MonadTrans` instance also a `MonadOhua` instance
instance {-# OVERLAPPABLE #-} (MonadOhua envExpr m, MonadTrans m0, Monad (m0 m)) => MonadOhua envExpr (m0 m) where
    recordWarning = lift . recordWarning
    onState = lift . onState
    failWith = lift . failWith


getState :: MonadOhua envExprs m => m (CompilerState envExprs)
getState = onState (\s -> (s, s))

fromState :: MonadOhua envExprs m => Lens' (CompilerState envExprs) a -> m a
fromState l = (^. l) <$> getState

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runOhuaT :: Monad ctxt => (Expression -> OhuaT env ctxt result) -> Expression -> ctxt (Either Error (result, Warnings))
runOhuaT f tree = runOhuaT0 (f tree) $ HS.fromList $ extractBindings tree

runOhuaT0 :: Monad ctxt => OhuaT env ctxt result -> HS.HashSet Binding -> ctxt (Either Error (result, Warnings))
runOhuaT0 f taken =
    runExceptT (evalRWST (runOhuaT' f) (error "Ohua has no environment!") (CompilerState nameGen 0 mempty))
  where
    nameGen = initNameGen taken


runOhuaT0IO :: MonadIO ctxt => OhuaT env ctxt result -> HS.HashSet Binding -> ctxt (Either Error result)
runOhuaT0IO f taken =  runOhuaT0 f taken >>= \case
    Left err -> return $ Left err
    Right (val, errors) -> do
        unless (null errors) $ liftIO $ T.putStrLn $ T.intercalate "\n" errors
        return $ Right val

initNameGen :: HS.HashSet Binding -> NameGenerator
initNameGen taken = NameGenerator taken
    [ Binding $ T.pack $ char : maybe [] show num
    | num <- Nothing : map Just [(0 :: Integer)..]
    , char <- ['a'..'z']
    ]

generateBinding :: MonadOhua envExpr m => m Binding
generateBinding = onState $ \s ->
    let taken = s ^. nameGenerator . takenNames
        (h:t) = dropWhile (`HS.member` taken) $ s ^. nameGenerator . simpleNameList
    in  ( h
        , s & nameGenerator . simpleNameList .~ t
            & nameGenerator . takenNames %~ HS.insert h
        )

generateBindingWith :: MonadOhua envExpr m => Binding -> m Binding
generateBindingWith (Binding prefix) = onState $ \s ->
    let taken = s ^. nameGenerator . takenNames
        (h:_) = dropWhile (`HS.member` taken) $ map (Binding . (prefix' <>) . T.pack . show) ([0..] :: [Int])
    in (h, s & nameGenerator . takenNames %~ HS.insert h)
  where prefix' = prefix <> "_"

generateId :: MonadOhua envExpr m => m FnId
generateId = onState $ \s ->
    (FnId $ s ^. idCounter + 1, s & idCounter %~ succ)


lookupEnvExpr :: MonadOhua envExpr m => HostExpr -> m (Maybe envExpr)
lookupEnvExpr (HostExpr i) = (^? envExpressions . ix i) <$> getState


addEnvExpression :: MonadOhua envExpr m => envExpr -> m HostExpr
addEnvExpression expr = onState $ \s ->
    ( HostExpr $ V.length $ s ^. envExpressions
    , s & envExpressions %~ (`V.snoc` expr)
    )

