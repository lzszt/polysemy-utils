{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Logging (
  Severity (..),
  parseSeverity,
  Log,
  LogConfig,
  LogConfigF (..),
  WithCtx (..),
  defaultLogConfig,
  runLogging,
  runLoggingWithTime,
  runLoggingWithProfilingTime,
  disableLogging,
  errorToFatalLog,
  errorToLog,
  logDebug,
  logInfo,
  logWarning,
  logError,
  logFatal,
  failToLog,
  failToFatalLog,
) where

import Colog.Polysemy.Effect qualified as Log
import Control.Monad
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy
import Data.Time
import Effects.Time
import Polysemy
import Polysemy.Error qualified as Error
import Polysemy.Fail qualified as Fail

-- | Severity of the log message
data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Fatal
  deriving (Show, Eq, Ord, Read)

-- | Convert a `String` into a `Severity`
parseSeverity :: String -> Maybe Severity
parseSeverity s =
  case reads s of
    [(severity, "")] -> Just severity
    _ -> Nothing

data LogMessage = LogMessage
  { logMessageSeverity :: Severity
  , logMessageText :: String
  }

-- | Type of the logging effect
type Log = Log.Log LogMessage

-- | A config for logging.
-- Contains a context `String` and a minimun `Severity`.
data LogConfigF f = LogConfig
  { logConfigContext :: f String
  , logConfigMinimumSeverity :: Severity
  }

instance Show LogConfig where
  show (LogConfig ctx minSeverity) =
    "LogConfig { logConfigContext = "
      <> show (NonEmpty.toList ctx)
      <> ", logConfigMinimumSeverity = "
      <> show minSeverity
      <> " }"

-- | Default logging config with minimum severity `Error`
-- and no context.
-- Apply a context using `addContext` to get a `LogConfig`.
defaultLogConfig :: LogConfigF Proxy
defaultLogConfig = LogConfig Proxy Error

class WithCtx c where
  addContext :: LogConfigF c -> String -> LogConfig

instance WithCtx Proxy where
  addContext logConfig ctx = logConfig{logConfigContext = ctx NonEmpty.:| []}

instance WithCtx NonEmpty where
  addContext logConfig ctx = logConfig{logConfigContext = ctx NonEmpty.<| logConfigContext logConfig}

-- | A complete `LogConfig` which can be used to run
-- the `Log` effect.
type LogConfig = LogConfigF NonEmpty

-- | Log a message with severity `Debug`
logDebug :: (Members '[Log] r) => String -> Sem r ()
logDebug = Log.log . LogMessage Debug

-- | Log a message with severity `Info`
logInfo :: (Members '[Log] r) => String -> Sem r ()
logInfo = Log.log . LogMessage Info

-- | Log a message with severity `Warning`
logWarning :: (Members '[Log] r) => String -> Sem r ()
logWarning = Log.log . LogMessage Warning

-- | Log a message with severity `Error`
logError :: (Members '[Log] r) => String -> Sem r ()
logError = Log.log . LogMessage Error

-- | Log a message with severity `Fatal`
logFatal :: (Members '[Log] r) => String -> Sem r ()
logFatal = Log.log . LogMessage Fatal

data TimingMode
  = NoTiming
  | DateTime UTCTime
  | PrecisionDateTime UTCTime

logPutStrLn :: (Members '[Embed IO] r) => LogConfig -> LogMessage -> TimingMode -> Sem r ()
logPutStrLn logConfig logMsg
  | logMessageSeverity logMsg >= logConfigMinimumSeverity logConfig =
      ( embed
          . putStrLn
          . ( \formattedTime ->
                formatLogSeverity (logMessageSeverity logMsg)
                  <> " "
                  <> formatLogContext (logConfigContext logConfig)
                  <> " "
                  <> formattedTime
                  <> logMessageText logMsg
            )
      )
        . \case
          PrecisionDateTime now -> formatProfilingTimeForLog now <> " "
          DateTime now -> formatTimeForLog now <> " "
          NoTiming -> ""
  | otherwise = const $ pure ()
  where
    formatTimeForLog = squareBrackets . formatTime defaultTimeLocale "%F %T"
    formatProfilingTimeForLog = squareBrackets . formatTime defaultTimeLocale "%F %T%Q"
    squareBrackets s = "[" <> s <> "]"
    formatLogSeverity = take 9 . (<> replicate 3 ' ') . squareBrackets . show
    formatLogContext = squareBrackets . intercalate ", " . NonEmpty.toList

-- | Runs the `Log` effect according to the `LogConfig` given.
-- Adds a timestamp to the log output with up to 12 decimal seconds.
runLoggingWithProfilingTime :: (Members '[Embed IO, Time] r) => LogConfig -> Sem (Log : r) a -> Sem r a
runLoggingWithProfilingTime logConfig = interpret $
  \(Log.Log logMsg) -> logPutStrLn logConfig logMsg . PrecisionDateTime =<< getTime

-- | Runs the `Log` effect according to the `LogConfig` given.
-- Adds a timestamp to the log output.
runLoggingWithTime :: (Members '[Embed IO, Time] r) => LogConfig -> Sem (Log : r) a -> Sem r a
runLoggingWithTime logConfig = interpret $
  \(Log.Log logMsg) -> logPutStrLn logConfig logMsg . DateTime =<< getTime

-- | Runs the `Log` effect according to the `LogConfig` given.
runLogging :: (Members '[Embed IO] r) => LogConfig -> Sem (Log : r) a -> Sem r a
runLogging logConfig = interpret $
  \(Log.Log logMsg) -> logPutStrLn logConfig logMsg NoTiming

-- | Suppresses the log message.
disableLogging :: Sem (Log : r) a -> Sem r a
disableLogging = interpret $ \Log.Log{} -> pure ()

-- | Convert an `Error` into a log message with `Fatal` severity.
errorToFatalLog ::
  (Show e) =>
  (Members '[Log] r) =>
  Sem (Error.Error e : r) a ->
  Sem r a
errorToFatalLog =
  Error.runError
    >=> \case
      Left e -> do
        logFatal $ show e
        error $ show e
      Right x -> pure x

-- | Convert an `Error` into a log message with `Error` severity.
errorToLog ::
  (Show e) =>
  (Members '[Log] r) =>
  Sem (Error.Error e : r) () ->
  Sem r ()
errorToLog =
  Error.runError
    >=> \case
      Left e ->
        logError $ show e
      Right x -> pure x

failToLog ::
  (Members '[Log] r) =>
  Sem (Fail.Fail : r) () ->
  Sem r ()
failToLog =
  Fail.runFail >=> \case
    Left e -> logError e
    Right x -> pure x

failToFatalLog ::
  (Members '[Log] r) =>
  Sem (Fail.Fail : r) a ->
  Sem r a
failToFatalLog =
  Fail.runFail >=> \case
    Left e -> do
      logFatal e
      error e
    Right x -> pure x
