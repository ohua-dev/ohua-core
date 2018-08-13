-- |
-- Module      : $Header$
-- Description : Utility functions and type synonyms
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

#include "compat.h"

#if !NEW_CALLSTACK_API
{-# LANGUAGE ImplicitParams, ConstraintKinds #-}
#endif
module Ohua.Util
    ( Prism
    , Prism'
    , prism
    , prism'
    , panicS
    , assertM
    , assertE
    , mapLeft
    , ShowT(..)
    , trace
    , traceShowId
    , forceAndReport
    , forceTraceReport
    , intentionally_not_implemented
    , Mutator(..)
    , tellMut
    , HasCallStack
    , callStack
    , callStackToStr
    , throwErrorS
    ) where

import Protolude

import Control.Exception
import Control.Monad.Writer
import Data.String
import qualified Data.Text as T
import Lens.Micro hiding ((<&>))

SEMIGROUP_COMPAT_IMPORT


-- | Similar to prisms from lens, but implemented with traversals.
type Prism s t a b = Traversal s t a b


-- | A 'Prism' where the types can't change by using the setter,
type Prism' a b = Prism a a b b


-- | Create a 'Prism' from a getter and a setter.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism make getter f thing =
    case getter thing of
        Left thing' -> pure thing'
        Right b     -> make <$> f b


-- | Create a 'Prism'' from a getter and a setter.
prism' :: (b -> a) -> (a -> Maybe b) -> Prism' a b
prism' make getter = prism make (\s -> maybe (Left s) Right $ getter s)


-- | Throw an 'error' if the condition is not met. The difference to
-- 'assert' is that the error is tied to @m@ and is not lazy as it
-- would be when using @pure $ assert cond val@.
assertM :: Applicative m => Bool -> m ()
assertM = flip assert (pure ())
{-# INLINE assertM #-}


-- | Similar to 'assertM' but throws a canonical 'Error' in the monad
-- @m@ rather than an 'error'.
assertE :: MonadError String m => Bool -> m ()
assertE True  = return ()
assertE False = throwError "AssertionError"
{-# INLINE assertE #-}


-- | Apply a function to the left value in an 'Either'
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left l)  = Left $ f l
mapLeft _ (Right r) = Right r
{-# INLINE mapLeft #-}


-- | Convert values to 'T.Text'
class ShowT a where
    showT :: a -> T.Text

instance {-# OVERLAPPABLE #-} Show a => ShowT a where
    showT = T.pack . show


-- | Force the evaluation of the value and then print a message to stdout.
forceAndReport :: (MonadIO m, NFData a) => String -> a -> m ()
forceAndReport msg val = val `deepseq` liftIO (putStrLn msg)


-- | Force the evaluation of a value and then print a message using 'trace'.
forceTraceReport :: (Applicative f, NFData a) => String -> a -> f ()
forceTraceReport msg val = val `deepseq` trace msg (pure ())


-- | An 'error' call to indicate a certain class method is
-- intentionally not implemented. Mostly used when 'Num' is
-- implemented for a type to get the overloaded integer syntax but the
-- type does not actually support numeric operations.
intentionally_not_implemented :: a
intentionally_not_implemented =
    panic "This is intentionally not implemented, don't use it!"



-- | This type only exists to overwrite the implementation of 'Monoid'
-- for functions.  It changes 'mappend' to be '(.)'. This makes it
-- possible to accumulate a chain of functions in a
-- 'Control.Monad.Writer.MonadWriter'.
newtype Mutator a = Mutator { mutAsFn :: a -> a }

#if BASE_HAS_SEMIGROUP
instance SG.Semigroup (Mutator a) where
  Mutator m1 <> Mutator m2 = Mutator $ m1 . m2
#endif

instance Monoid (Mutator a) where
  mempty = Mutator identity

#if BASE_HAS_SEMIGROUP
  mappend = (SG.<>)
#else
  Mutator m1 `mappend` Mutator m2 = Mutator $ m1 . m2
#endif


-- | Append a function to a chain in a writer.
tellMut :: MonadWriter (Mutator a) m => (a -> a) -> m ()
tellMut = tell . Mutator



#if !NEW_CALLSTACK_API
-- | Backport of the new callstack API.
type HasCallStack = ?callStack :: CallStack

-- | Backport of the new callstack API.
callStack :: HasCallStack => CallStack
callStack = ?callStack
#endif

-- | Get a readable representation of a callstack.
callStackToStr :: CallStack -> String
#if NEW_CALLSTACK_API
callStackToStr = prettyCallStack
#else
callStackToStr = showCallStack
#endif


-- | Throw a canonical compiler error with a callstack appended.
throwErrorS :: (HasCallStack, MonadError s m, IsString s, Monoid s) => s -> m a
throwErrorS msg = throwError $ msg <> "\n" <> fromString cs
  where
    cs = callStackToStr callStack

panicS :: HasCallStack => Text -> a
panicS msg = panic $ msg <> "\n" <> cs
  where
    cs = toS $ callStackToStr callStack
