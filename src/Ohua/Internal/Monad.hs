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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Ohua.Internal.Monad where


import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Error  as E
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Control.Natural
import qualified Data.HashSet               as HS
import           Data.Monoid
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

data GenBnd r where
  GenerateBinding :: GenBnd Binding
  GenerateBindingWith :: Binding -> GenBnd Binding

generateBinding :: Member GenBnd effs => Eff effs Binding
generateBinding = send GenerateBinding

generateBindingWith :: Member GenBnd effs => Binding -> Eff effs Binding
generateBindingWith = send . GenerateBindingWith

deepseqM :: (Monad m, NFData a) => a -> m ()
deepseqM a = a `deepseq` pure ()

runGenBnd :: NameGenerator -> Eff (GenBnd ': effs) ~> Eff effs
runGenBnd initS = evalState initS . genToState
  where
    genToState :: Eff (GenBnd ': effs) ~> Eff (State NameGenerator ': effs)
    genToState = reinterpret $ \case
      GenerateBinding -> do
        g <- get
        let taken = g ^. takenNames
            (h:t) = dropWhile (`HS.member` taken) (g ^. simpleNameList)
            g' = g & simpleNameList .~ t
                   & takenNames %~ HS.insert h
        put g'
        pure h
      GenerateBindingWith (Binding prefix) -> do
        g <- get
        let taken = g ^. takenNames
            prefix' = prefix <> "_"
            (h:_) = dropWhile (`HS.member` taken) $ map (Binding . (prefix' <>) . Str.showS) ([0..] :: [Int])
            g' = g & takenNames %~ HS.insert h
        put g'
        pure h

data GenId r where
    GenerateId :: GenId FnId
    -- | Unsafe. Only use this if you know what you are doing!!
    ResetIdCounter :: FnId -> GenId ()

generateId :: Member GenId effs => Eff effs FnId
generateId = send GenerateId

resetIdCounter :: Member GenId effs => FnId -> Eff effs ()
resetIdCounter = send . ResetIdCounter

runGenId :: FnId -> Eff (GenId ': effs) ~> Eff effs
runGenId i = evalState i . genToState
  where
    genToState :: Eff (GenId ': effs) ~> Eff (State FnId ': effs)
    genToState = reinterpret $ \case
      GenerateId -> modify (succ :: FnId -> FnId) >> get
      ResetIdCounter val -> put val


data ReadEnvExpr expr r where
    LookupEnvExpr :: HostExpr -> ReadEnvExpr expr (Maybe expr)

lookupEnvExpr :: Member (ReadEnvExpr expr) effs => HostExpr -> Eff effs (Maybe expr)
lookupEnvExpr = send . LookupEnvExpr

runReadEnvExpr :: forall effs expr . Member (State (V.Vector expr)) effs
               => Eff (ReadEnvExpr expr ': effs) ~> Eff effs
runReadEnvExpr = interpret $ \case
  LookupEnvExpr (HostExpr i) -> (^? ix i) <$> (get :: Eff effs (V.Vector expr))

getEnvExpr :: Members '[E.Error Ty.Error, ReadEnvExpr expr] effs => HostExpr -> Eff effs expr
getEnvExpr = maybe (throwError msg) pure <=< lookupEnvExpr
  where msg = "Invariant violated, host expression was not defined." :: Ty.Error

data RecordEnvExpr expr r where
  AddEnvExpression :: expr -> RecordEnvExpr expr HostExpr

addEnvExpression :: Member (RecordEnvExpr expr) effs => expr -> Eff effs HostExpr
addEnvExpression = send . AddEnvExpression

runRecordEnvExpr :: forall effs expr . Member (State (V.Vector expr)) effs
                 => Eff (RecordEnvExpr expr ': effs) ~> Eff effs
runRecordEnvExpr = interpret $ \case
  AddEnvExpression e -> do
    modify (`V.snoc` e)
    HostExpr . (V.length :: V.Vector expr -> Int) <$> get

type Ohua effs = Members OhuaEffList effs

type OhuaErr = E.Error Ty.Error

type OhuaEffList = '[ GenId
                    , GenBnd
                    , OhuaErr
                    , IO
                    , Reader Environment
                    , Logger
                    ]

type OhuaEffs expr effs = GenBnd ': GenId ': ReadEnvExpr expr ': RecordEnvExpr expr ': Reader Environment ': State (V.Vector expr) ': effs

-- | Run a compiler
-- Creates the state from the tree being passed in
-- If there are any errors during the compilation they are reported together at the end
runFromExpr :: Members '[Logger, IO, E.Error Ty.Error] effs
            => Options
            -> (Expression -> Eff (OhuaEffs env effs) result)
            -> Expression
            -> Eff effs result
runFromExpr opts f tree = runFromBindings opts (f tree) $ HS.fromList $ extractBindings tree

runFromBindings :: forall env effs result . Members '[Logger, IO, E.Error Ty.Error] effs
                => Options
                -> Eff (OhuaEffs env effs) result
                -> HS.HashSet Binding
                -> Eff effs result
runFromBindings opts f taken
  = evalState (mempty :: V.Vector env)
  $ runReader env
  $ runRecordEnvExpr
  $ runReadEnvExpr
  $ runGenId 0
  $ runGenBnd nameGen
  $ f
  where
    nameGen = initNameGen taken
    env = Environment opts


initNameGen :: HS.HashSet Binding -> NameGenerator
initNameGen taken = NameGenerator taken
    [ Binding $ Str.fromString $ char : maybe [] show num
    | num <- Nothing : map Just [(0 :: Integer)..]
    , char <- ['a'..'z']
    ]
