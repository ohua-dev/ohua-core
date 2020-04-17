-- |
-- Module      : $Header$
-- Description : Utility functions and type synonyms
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

#include "compat.h"

#if !NEW_CALLSTACK_API
{-# LANGUAGE ImplicitParams#-}
#endif
module Ohua.Util
    ( Prism
    , Prism'
    , prism
    , prism'
    , assertM
    , assertE
    , mapLeft
    , ShowT(..)
    , trace
    , traceShowId
    , forceAndReport
    , forceTraceReport
    , intentionally_not_implemented
    , isDebug
    , debugOr
    , whenDebug
    , unlessDebug
    , dAssert
    , dAssertM
    , dAssertE
    , Mutator(..)
    , tellMut
    , HasCallStack
    , callStack
    , callStackToStr
    , throwErrorS
    , throwErrorDebugS
    , (<&>)
    , SemigroupConstraint
    , flexText
    , quickRender
    , ohuaDefaultLayoutOpts
    , errorD
    ) where

import Universum

import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Error.Class
import Control.Exception.Safe
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

#if !NEW_CALLSTACK_API
import GHC.Stack
#endif


-- | Boolean indicating whether this is a debug build
isDebug :: Bool
isDebug =
#if DEBUG
  True
#else
  False
#endif
{-# INLINE isDebug #-}

-- | @a `debugOr` b@ executed @a@ in a debug build, otherwise @b@
debugOr :: a -> a -> a
debugOr a0 a1
  | isDebug = a0
  | otherwise = a1
{-# INLINE debugOr #-}

-- | Run an action only if this is a debug build
whenDebug :: Applicative f => f () -> f ()
whenDebug = when isDebug
{-# INLINE whenDebug #-}

-- | Run an action only if this is _not_ a debug build
unlessDebug :: Applicative f => f () -> f ()
unlessDebug = unless isDebug
{-# INLINE unlessDebug #-}

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
assertE :: (IsString s, MonadError s m, HasCallStack, Monoid s) => Bool -> m ()
assertE True  = return ()
assertE False = throwErrorDebugS "AssertionError"
{-# INLINE assertE #-}

-- | Checks the condition only when debug flags are enabled.
dAssert :: HasCallStack => Bool -> a -> a
dAssert = assert `debugOr` flip const
{-# INLINE dAssert #-}

-- | Like 'assertM' but only checks the condition in debug builds.
dAssertM :: (HasCallStack, Applicative m) => Bool -> m ()
dAssertM = assertM `debugOr` const (pure ())
{-# INLINE dAssertM #-}

-- | Like 'assertE' but only checks the condition in debug builds.
dAssertE :: (IsString s, MonadError s m, HasCallStack, Monoid s) => Bool -> m ()
dAssertE = assertE `debugOr` const (pure ())
{-# INLINE dAssertE #-}

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
forceTraceReport msg val = val `deepseq` trace (T.pack msg) (pure ())


-- | An 'error' call to indicate a certain class method is
-- intentionally not implemented. Mostly used when 'Num' is
-- implemented for a type to get the overloaded integer syntax but the
-- type does not actually support numeric operations.
intentionally_not_implemented :: a
intentionally_not_implemented =
    error "This is intentionally not implemented, don't use it!"



-- | This type only exists to overwrite the implementation of 'Monoid'
-- for functions.  It changes 'mappend' to be '(.)'. This makes it
-- possible to accumulate a chain of functions in a
-- 'Control.Monad.Writer.MonadWriter'.
type Mutator = Endo

mutAsFn :: Mutator a -> a -> a
mutAsFn = appEndo

-- | Append a function to a chain in a writer.
tellMut :: MonadWriter (Endo a) m => (a -> a) -> m ()
tellMut = tell . Endo


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

type SemigroupConstraint f = Monoid f

-- | Throw a canonical compiler error with a callstack appended.
throwErrorS :: (HasCallStack, MonadError s m, IsString s, SemigroupConstraint s) => s -> m a
throwErrorS msg = throwError $ msg <> "\n" <> fromString cs
  where
    cs = callStackToStr callStack

-- | Throws a canonical compiler error which, in case of a debug build, has a
-- call stack attached.
throwErrorDebugS :: (HasCallStack, MonadError s m, IsString s, SemigroupConstraint s) => s -> m a
throwErrorDebugS = throwErrorS `debugOr` throwError
{-# INLINE throwErrorDebugS #-}

-- | Throw a canonical compiler error with a callstack appended.
throwErrorSD :: (HasCallStack, MonadError Text m) => PP.Doc ann -> m a
throwErrorSD msg = throwError $ ohuaDefaultLayoutDoc $ PP.vcat [msg, fromString cs]
  where
    cs = callStackToStr callStack

throwErrorD :: MonadError Text m => PP.Doc ann -> m a
throwErrorD = throwError . ohuaDefaultLayoutDoc

-- | Throws a canonical compiler error which, in case of a debug build, has a
-- call stack attached.
throwErrorDebugSD :: (HasCallStack, MonadError Text m)  => PP.Doc ann -> m a
throwErrorDebugSD = throwErrorSD `debugOr` throwErrorD
{-# INLINE throwErrorDebugSD #-}

ohuaDefaultLayoutOpts :: PP.LayoutOptions
ohuaDefaultLayoutOpts =
    PP.defaultLayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine 100 1.0}

ohuaDefaultLayoutDoc :: PP.Doc ann -> Text
ohuaDefaultLayoutDoc = PP.renderStrict . PP.layoutSmart ohuaDefaultLayoutOpts

quickRender :: PP.Pretty a => a -> Text
quickRender = ohuaDefaultLayoutDoc . PP.pretty

-- | Throws an error with a document as message
errorD :: PP.Doc ann -> a
errorD = error . ohuaDefaultLayoutDoc

-- | Takes a text and converts it to a doc that preserves line breaks, but also
-- concatenates words such that lines that are too long will automatically break.
flexText :: Text -> PP.Doc ann
flexText = PP.vsep . map (PP.fillSep . map PP.pretty . words) . lines
