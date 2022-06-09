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

module Effects.Delay (
  Delay (..),
  delay,
  runDelay,
) where

import Control.Concurrent
import Data.Time
import Polysemy

data Delay r a where
  Delay :: NominalDiffTime -> Delay r ()

makeSem ''Delay

runDelay :: Members '[Embed IO] r => Sem (Delay : r) a -> Sem r a
runDelay =
  interpret $ \case
    Delay d -> embed $ threadDelay $ round $ 1_000_000 * nominalDiffTimeToSeconds d
