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
module Ohua.Util where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Text            as T
import           Lens.Micro
import           System.IO
import           System.IO.Unsafe
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.Semigroup       as SG
#endif

type Prism s t a b = Traversal s t a b


type Prism' a b = Prism a a b b


prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism make get f thing =
    case get thing of
        Left thing' -> pure thing'
        Right b     -> make <$> f b


prism' :: (b -> a) -> (a -> Maybe b) -> Prism' a b
prism' make get = prism make (\s -> maybe (Left s) Right $ get s)



assertM :: Applicative m => Bool -> m ()
assertM = flip assert (pure ())
{-# INLINE assertM #-}


assertE :: MonadError String m => Bool -> m ()
assertE True  = return ()
assertE False = throwError "AssertionError"
{-# INLINE assertE #-}


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left l)  = Left $ f l
mapLeft _ (Right r) = Right r


class ShowT a where
    showT :: a -> T.Text

instance {-# OVERLAPPABLE #-} Show a => ShowT a where
    showT = T.pack . show

trace :: String -> a -> a
trace msg a = unsafePerformIO (hPutStrLn stderr msg >> pure a)
{-# NOINLINE trace #-}


traceShowId :: Show a => a -> a
traceShowId a = trace (show a) a

forceAndReport :: (MonadIO m, NFData a) => String -> a -> m ()
forceAndReport msg val = val `deepseq` liftIO (putStrLn msg)


forceTraceReport :: (Applicative f, NFData a) => String -> a -> f ()
forceTraceReport msg val = val `deepseq` trace msg (pure ())


intentionally_not_implemented :: a
intentionally_not_implemented = error "This is intentionally not implemented, don't use it!"



-- | This type onlt exists to overwrite the implementation of 'Monoid' for functions.
-- It changes 'mappend' to be '(.)'. This makes it possible to accumulate a chain of functions in a
-- 'Control.Monad.Writer.MonadWriter'.
newtype Mutator a = Mutator { mutAsFn :: a -> a }

#if __GLASGOW_HASKELL__ >= 800
instance SG.Semigroup (Mutator a) where
  Mutator m1 <> Mutator m2 = Mutator $ m1 . m2
#endif

instance Monoid (Mutator a) where
  mempty = Mutator id

#if __GLASGOW_HASKELL__ >= 800
  mappend = (SG.<>)
#else
  Mutator m1 `mappend` Mutator m2 = Mutator $ m1 . m2
#endif
