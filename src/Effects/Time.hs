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
  )
where

import Data.Time
import Polysemy

data Time r a where
  GetTime :: Time r UTCTime

makeSem ''Time

runTime :: Members '[Embed IO] r => Sem (Time : r) a -> Sem r a
runTime =
  interpret $ \case
    GetTime -> embed getCurrentTime
