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

module Effects.Time
  ( Time (..),
    getTime,
    runTime,
    runFixedTime,
  )
where

import Data.Time
import Polysemy

-- | An effect for the current `UTCTime`
data Time r a where
  GetTime :: Time r UTCTime

makeSem ''Time

-- | Run `Time` effect via `Embed IO`
runTime :: Members '[Embed IO] r => Sem (Time : r) a -> Sem r a
runTime =
  interpret $ \case
    GetTime -> embed getCurrentTime

-- | Run `Time` effect with a fixed `UTCTime`
runFixedTime :: UTCTime -> Sem (Time : r) a -> Sem r a
runFixedTime fixedTime =
  interpret $ \GetTime -> pure fixedTime
