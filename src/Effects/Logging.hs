{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Logging
  ( Severity (..),
    parseSeverity,
    Log,
    LogConfig,
    LogConfigF (..),
    WithCtx (..),
    defaultLogConfig,
    runLogging,
    runLoggingWithTime,
    disableLogging,
    errorToFatalLog,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logFatal,
  )
where

import qualified Colog.Polysemy.Effect as Log
import Control.Monad
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy
import Data.Time
import Effects.Time
import Polysemy
import qualified Polysemy.Error as Error

data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Fatal
  deriving (Show, Eq, Ord, Read)

parseSeverity :: String -> Maybe Severity
parseSeverity s =
  case reads s of
    [(severity, "")] -> Just severity
    _ -> Nothing

data LogMessage = LogMessage
  { logMessageSeverity :: Severity,
    logMessageText :: String
  }

type Log = Log.Log LogMessage

data LogConfigF f = LogConfig
  { logConfigContext :: f String,
    logConfigMinimumSeverity :: Severity
  }

instance Show LogConfig where
  show (LogConfig ctx minSeverity) = "LogConfig { logConfigContext = " <> show (NonEmpty.toList ctx) <> ", logConfigMinimumSeverity = " <> show minSeverity <> " }"

defaultLogConfig :: LogConfigF Proxy
defaultLogConfig = LogConfig Proxy Error

class WithCtx c where
  addContext :: LogConfigF c -> String -> LogConfig

instance WithCtx Proxy where
  addContext logConfig ctx = logConfig {logConfigContext = ctx NonEmpty.:| []}

instance WithCtx NonEmpty where
  addContext logConfig ctx = logConfig {logConfigContext = ctx NonEmpty.<| logConfigContext logConfig}

type LogConfig = LogConfigF NonEmpty

logDebug :: Members '[Log] r => String -> Sem r ()
logDebug = Log.log . LogMessage Debug

logInfo :: Members '[Log] r => String -> Sem r ()
logInfo = Log.log . LogMessage Info

logWarning :: Members '[Log] r => String -> Sem r ()
logWarning = Log.log . LogMessage Warning

logError :: Members '[Log] r => String -> Sem r ()
logError = Log.log . LogMessage Error

logFatal :: Members '[Log] r => String -> Sem r ()
logFatal = Log.log . LogMessage Fatal

logPutStrLn :: Members '[Embed IO] r => LogConfig -> LogMessage -> Maybe UTCTime -> Sem r ()
logPutStrLn logConfig logMsg = \case
  Just now ->
    when (logMessageSeverity logMsg >= logConfigMinimumSeverity logConfig) $
      embed $
        putStrLn $
          formatLogSeverity (logMessageSeverity logMsg)
            <> " "
            <> formatLogContext (logConfigContext logConfig)
            <> " "
            <> formatTimeForLog now
            <> " "
            <> logMessageText logMsg
  Nothing ->
    when (logMessageSeverity logMsg >= logConfigMinimumSeverity logConfig) $
      embed $
        putStrLn $
          formatLogSeverity (logMessageSeverity logMsg)
            <> " "
            <> formatLogContext (logConfigContext logConfig)
            <> " "
            <> logMessageText logMsg
  where
    formatTimeForLog = squareBrackets . formatTime defaultTimeLocale "%F %T"
    squareBrackets s = "[" <> s <> "]"
    formatLogSeverity = take 9 . (<> replicate 3 ' ') . squareBrackets . show
    formatLogContext = squareBrackets . intercalate ", " . NonEmpty.toList

runLoggingWithTime :: Members '[Embed IO, Time] r => LogConfig -> Sem (Log : r) a -> Sem r a
runLoggingWithTime logConfig = interpret $
  \(Log.Log logMsg) -> logPutStrLn logConfig logMsg . Just =<< getTime

runLogging :: Members '[Embed IO] r => LogConfig -> Sem (Log : r) a -> Sem r a
runLogging logConfig = interpret $
  \(Log.Log logMsg) -> logPutStrLn logConfig logMsg Nothing

disableLogging :: Sem (Log : r) a -> Sem r a
disableLogging = interpret $ \Log.Log {} -> pure ()

errorToFatalLog ::
  Show e =>
  Members '[Log] r =>
  Sem (Error.Error e : r) a ->
  Sem r a
errorToFatalLog =
  Error.runError
    >=> \case
      Left e -> do
        logFatal $ show e
        error $ show e
      Right x -> pure x
