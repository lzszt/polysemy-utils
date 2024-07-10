{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Time (
  Time (..),
  getTime,
  Effects.Time.getCurrentTimeZone,
  runTime,
  runFixedTime,
  runFakeTime,
) where

import Data.Time
import Effects.Delay
import Polysemy
import Polysemy.State

-- | An effect for the current `UTCTime`
data Time r a where
  GetTime :: Time r UTCTime
  GetCurrentTimeZone :: Time r TimeZone

makeSem ''Time

-- | Run `Time` effect via `Embed IO`
runTime :: (Members '[Embed IO] r) => Sem (Time : r) a -> Sem r a
runTime =
  interpret $ \case
    GetTime -> embed getCurrentTime
    GetCurrentTimeZone -> embed Data.Time.getCurrentTimeZone

-- | Testing interpretations

-- | Run `Time` effect with a fixed `UTCTime`
runFixedTime :: (UTCTime, TimeZone) -> Sem (Time : r) a -> Sem r a
runFixedTime (fixedTime, fixedTimeZone) =
  interpret $ \case
    GetTime -> pure fixedTime
    GetCurrentTimeZone -> pure fixedTimeZone

-- | Runs `Time` as fixed to given `UTCTime`
-- but advances time whenever a `Delay` effect is encountered
runFakeTime :: (UTCTime, TimeZone) -> Sem (Time : Delay : State UTCTime : r) a -> Sem r a
runFakeTime (startTime, timeZone) =
  evalState startTime
    . interpret (\(Delay d) -> modify (addUTCTime d))
    . interpret
      ( \case
          GetTime -> get
          GetCurrentTimeZone -> pure timeZone
      )
