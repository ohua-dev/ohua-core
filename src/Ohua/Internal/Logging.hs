-- This is a temporary module which contains code from the @monad-logger@ library.
-- It is intended to be dropped for a direct dependency on @monad-logger@ when it becomes compatible
-- with eta.
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
module Ohua.Internal.Logging where


import           Control.Monad.Base
import           Control.Monad.Except        (ExceptT, MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (MonadReader (..), ReaderT)
import           Control.Monad.RWS.Lazy      (MonadRWS, RWST)
import qualified Control.Monad.RWS.Strict    as Strict
import           Control.Monad.State.Lazy    (MonadState (..), StateT)
import qualified Control.Monad.State.Strict  as Strict
import qualified Control.Monad.Trans.Class   as Trans
import           Control.Monad.Trans.Control
import           Control.Monad.Writer        (MonadWriter (..), WriterT)
import qualified Data.ByteString             as BS
import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as BL
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import           System.IO                   (Handle, stderr)

#if __GLASGOW_HASKELL__ >= 800

import qualified Data.Semigroup              as SG

#endif


class Monad m => MonadLogger m where
    monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()
    default monadLoggerLog :: (MonadLogger m', Trans.MonadTrans t, ToLogStr msg, m ~ t m')
                           => Loc -> LogSource -> LogLevel -> msg -> m ()
    monadLoggerLog loc src lvl msg = Trans.lift $ monadLoggerLog loc src lvl msg

class (MonadLogger m, MonadIO m) => MonadLoggerIO m where
    -- | Request the logging function itself.
    --
    -- @since 0.3.10
    askLoggerIO :: m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    default askLoggerIO :: (Trans.MonadTrans t, MonadLoggerIO n, m ~ t n)
                        => m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    askLoggerIO = Trans.lift askLoggerIO

instance MonadLogger m => MonadLogger (ExceptT e m)
instance MonadLogger m => MonadLogger (ReaderT r m)
instance (MonadLogger m, Monoid w) => MonadLogger (WriterT w m)
instance MonadLogger m => MonadLogger (StateT s m)
instance MonadLogger m => MonadLogger (Strict.StateT s m)
instance (MonadLogger m, Monoid w) => MonadLogger (RWST r w s m)
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.RWST r w s m)

instance MonadLoggerIO m => MonadLoggerIO (ExceptT e m)
instance MonadLoggerIO m => MonadLoggerIO (ReaderT r m)
instance MonadLoggerIO m => MonadLoggerIO (StateT s m)
instance MonadLoggerIO m => MonadLoggerIO (Strict.StateT s m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (RWST r w s m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (Strict.RWST r w s m)

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther T.Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)


newtype LoggingT m a = LoggingT { runLoggingT :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> m a }

instance MonadError e m => MonadError e (LoggingT m) where
    throwError = Trans.lift . throwError
    catchError r h = LoggingT $ \i -> runLoggingT r i `catchError` \e -> runLoggingT (h e) i

mapLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
mapLoggingT f = LoggingT . (f .) . runLoggingT

filterLogger :: (LogSource -> LogLevel -> Bool)
             -> LoggingT m a
             -> LoggingT m a
filterLogger p (LoggingT f) = LoggingT $ \logger ->
    f $ \loc src level msg ->
        if (p src level) then
            logger loc src level msg
        else pure ()

instance MonadRWS r w s m => MonadRWS r w s (LoggingT m)

instance MonadReader r m => MonadReader r (LoggingT m) where
    ask = Trans.lift ask
    local = mapLoggingT . local


instance MonadState s m => MonadState s (LoggingT m) where
    get = Trans.lift get
    put = Trans.lift . put

instance MonadWriter w m => MonadWriter w (LoggingT m) where
    tell   = Trans.lift . tell
    listen = mapLoggingT listen
    pass   = mapLoggingT pass


instance Functor m => Functor (LoggingT m) where
    fmap f logger = LoggingT $ \loggerFn -> fmap f $ (runLoggingT logger) loggerFn
    {-# INLINE fmap #-}

instance Applicative m => Applicative (LoggingT m) where
    pure = LoggingT . const . pure
    {-# INLINE pure #-}
    loggerF <*> loggerA = LoggingT $ \loggerFn ->
                                       (runLoggingT loggerF) loggerFn
                                       <*> (runLoggingT loggerA) loggerFn
    {-# INLINE (<*>) #-}

instance Monad m => Monad (LoggingT m) where
    return = LoggingT . const . return
    LoggingT ma >>= f = LoggingT $ \r -> do
        a <- ma r
        let LoggingT f' = f a
        f' r

instance MonadIO m => MonadIO (LoggingT m) where
    liftIO = Trans.lift . liftIO

instance MonadBase b m => MonadBase b (LoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans LoggingT where
    lift = LoggingT . const

instance MonadTransControl LoggingT where
    type StT LoggingT a = a
    liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> t r
    restoreT = LoggingT . const
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
     type StM (LoggingT m) a = StM m a
     liftBaseWith f = LoggingT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(LoggingT r) -> r reader')
     restoreM = LoggingT . const . restoreM


instance MonadIO m => MonadLogger (LoggingT m) where
    monadLoggerLog a b c d = LoggingT $ \f -> liftIO $ f a b c (toLogStr d)

instance MonadIO m => MonadLoggerIO (LoggingT m) where
    askLoggerIO = LoggingT return

type Loc = ()
type LogSource = Text

defaultOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
defaultOutput h loc src level msg =
    S8.hPutStr h ls
  where
    ls = defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> S8.ByteString
defaultLogStrBS a b c d =
    toBS $ defaultLogStr a b c d
  where
    toBS = fromLogStr

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

defaultLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> LogStr
defaultLogStr _ src level msg =
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend`
    "\n"


runHandleLoggingT :: Handle -> LoggingT m a -> m a
runHandleLoggingT h = (`runLoggingT` defaultOutput h)


runStderrLoggingT :: LoggingT m a -> m a
runStderrLoggingT = runHandleLoggingT stderr

runSilentLoggingT :: LoggingT m a -> m a
runSilentLoggingT = flip runLoggingT $ \_ _ _ _ -> pure ()


data LogStr = LogStr !Int Builder

instance Monoid LogStr where
  mempty = LogStr 0 (toBuilder BS.empty)

#if __GLASGOW_HASKELL__ >= 800

  mappend = (SG.<>)

instance SG.Semigroup LogStr where
  LogStr s1 b1 <> LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)

#else
  LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)
#endif


instance IsString LogStr where
    fromString = toLogStr . TL.pack

class ToLogStr msg where
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    toLogStr = id
instance ToLogStr S8.ByteString where
    toLogStr bs = LogStr (BS.length bs) (toBuilder bs)
instance ToLogStr BL.ByteString where
    toLogStr = toLogStr . S8.concat . BL.toChunks
instance ToLogStr String where
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    toLogStr = toLogStr . T.encodeUtf8
instance ToLogStr TL.Text where
    toLogStr = toLogStr . TL.encodeUtf8

instance Show LogStr where
    show = show . T.decodeUtf8 . fromLogStr

instance Eq LogStr where
    a == b = fromLogStr a == fromLogStr b

defaultLoc :: Loc
defaultLoc = ()

-- | Obtaining the length of 'LogStr'.
logStrLength :: LogStr -> Int
logStrLength (LogStr n _) = n

-- | Converting 'LogStr' to 'ByteString'.
fromLogStr :: LogStr -> BS.ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder

toBuilder :: BS.ByteString -> Builder
toBuilder = B.byteString

fromBuilder :: Builder -> BS.ByteString
fromBuilder = BL.toStrict . B.toLazyByteString

logWithoutLoc :: (MonadLogger m, ToLogStr msg) => LogSource -> LogLevel -> msg -> m ()
logWithoutLoc = monadLoggerLog defaultLoc

logDebugN :: MonadLogger m => Text -> m ()
logDebugN = logWithoutLoc "" LevelDebug

logInfoN :: MonadLogger m => Text -> m ()
logInfoN = logWithoutLoc "" LevelInfo

logWarnN :: MonadLogger m => Text -> m ()
logWarnN = logWithoutLoc "" LevelWarn

logErrorN :: MonadLogger m => Text -> m ()
logErrorN = logWithoutLoc "" LevelError

logOtherN :: MonadLogger m => LogLevel -> Text -> m ()
logOtherN = logWithoutLoc ""

logDebugNS :: MonadLogger m => Text -> Text -> m ()
logDebugNS src = logWithoutLoc src LevelDebug

logInfoNS :: MonadLogger m => Text -> Text -> m ()
logInfoNS src = logWithoutLoc src LevelInfo

logWarnNS :: MonadLogger m => Text -> Text -> m ()
logWarnNS src = logWithoutLoc src LevelWarn

logErrorNS :: MonadLogger m => Text -> Text -> m ()
logErrorNS src = logWithoutLoc src LevelError

logOtherNS :: MonadLogger m => Text -> LogLevel -> Text -> m ()
logOtherNS = logWithoutLoc
