-- This is a temporary module which contains code from the @monad-logger@ library.
-- It is intended to be dropped for a direct dependency on @monad-logger@ when it becomes compatible
-- with eta.
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Ohua.Internal.Logging where


import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Control.Natural
import qualified Data.ByteString           as BS
import           Data.ByteString.Builder   (Builder)
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as BL
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           System.IO                 (Handle, stderr)


data Logger m where
    MonadLoggerLog :: Loc -> LogSource -> LogLevel -> LogStr -> Logger ()

monadLoggerLog :: (Member Logger effs, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> Eff effs ()
monadLoggerLog loc src lvl = send . MonadLoggerLog loc src lvl . toLogStr

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther T.Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)


runLogger :: Member IO effs => (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> Eff (Logger ': effs) ~> Eff effs
runLogger logFn = interpret $ \case
  MonadLoggerLog loc src lvl msg -> send $ (logFn loc src lvl msg :: IO ())

filterLogger :: (LogSource -> LogLevel -> Bool)
             -> Eff (Logger ': effs)
             ~> Eff (Logger ': effs)
filterLogger p = reinterpret $ \case
  l@(MonadLoggerLog loc src lvl msg) ->
    if p src lvl
       then send l
       else pure ()

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


runHandleLogger :: Member IO effs => Handle -> Eff (Logger ': effs) ~> Eff effs
runHandleLogger = runLogger . defaultOutput


runStderrLogger :: Member IO effs => Eff (Logger ': effs) a -> Eff effs a
runStderrLogger = runHandleLogger stderr

runSilentLogger :: Member IO effs => Eff (Logger ': effs) a -> Eff effs a
runSilentLogger = runLogger $ \_ _ _ _ -> pure ()


data LogStr = LogStr !Int Builder

instance Monoid LogStr where
    mempty = LogStr 0 (toBuilder BS.empty)
    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)

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

logWithoutLoc :: (Member Logger effs, ToLogStr msg) => LogSource -> LogLevel -> msg -> Eff effs ()
logWithoutLoc = monadLoggerLog defaultLoc

logDebugN :: Member Logger effs => Text -> Eff effs ()
logDebugN = logWithoutLoc "" LevelDebug

logInfoN :: Member Logger effs => Text -> Eff effs ()
logInfoN = logWithoutLoc "" LevelInfo

logWarnN :: Member Logger effs => Text -> Eff effs ()
logWarnN = logWithoutLoc "" LevelWarn

logErrorN :: Member Logger effs => Text -> Eff effs ()
logErrorN = logWithoutLoc "" LevelError

logOtherN :: Member Logger effs => LogLevel -> Text -> Eff effs ()
logOtherN = logWithoutLoc ""

logDebugNS :: Member Logger effs => Text -> Text -> Eff effs ()
logDebugNS src = logWithoutLoc src LevelDebug

logInfoNS :: Member Logger effs => Text -> Text -> Eff effs ()
logInfoNS src = logWithoutLoc src LevelInfo

logWarnNS :: Member Logger effs => Text -> Text -> Eff effs ()
logWarnNS src = logWithoutLoc src LevelWarn

logErrorNS :: Member Logger effs => Text -> Text -> Eff effs ()
logErrorNS src = logWithoutLoc src LevelError

logOtherNS :: Member Logger effs => Text -> LogLevel -> Text -> Eff effs ()
logOtherNS = logWithoutLoc
