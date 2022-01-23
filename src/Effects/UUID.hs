{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effects.UUID
  ( UUIDEff (..),
    getUUID,
    UUID,
    runUUIDEff,
    withConstantUUID,
  )
where

import Data.UUID
import Data.UUID.V4
import Polysemy

-- |
data UUIDEff r a where
  GetUUID :: UUIDEff r UUID

makeSem ''UUIDEff

runUUIDEff :: Members '[Embed IO] r => Sem (UUIDEff : r) a -> Sem r a
runUUIDEff = interpret $ \case
  GetUUID -> embed nextRandom

withConstantUUID :: UUID -> Sem (UUIDEff : r) a -> Sem r a
withConstantUUID constUUID = interpret $ \case
  GetUUID -> pure constUUID
