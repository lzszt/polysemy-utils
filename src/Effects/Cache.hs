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

module Effects.Cache
  ( Cache,
    CacheType,
    CacheStrategy (..),
    get,
    update,
    isValid,
    runCacheAsState,
  )
where

import qualified Data.Map.Strict as Map
import Data.Time
import Effects.Logging
import Effects.Time
import Polysemy
import qualified Polysemy.State as State

data Cache k v r a where
  Get :: k -> Cache k v r (Maybe v)
  Update :: k -> v -> Cache k v r ()

makeSem ''Cache

type CacheType k v = Map.Map k (UTCTime, v)

data CacheStrategy
  = Indefinitly
  | CacheFor NominalDiffTime
  deriving (Show)

isValid :: CacheStrategy -> UTCTime -> UTCTime -> Bool
isValid Indefinitly _ _ = True
isValid (CacheFor diff) cacheTime now = diffUTCTime now cacheTime <= diff

runCacheAsState ::
  (Members '[Time, Log] r) =>
  (Ord k) =>
  (Show k) =>
  CacheStrategy ->
  Sem (Cache k v : r) a ->
  Sem (State.State (CacheType k v) : r) a
runCacheAsState strat =
  reinterpret $ \case
    Get k -> do
      cacheEntry <- Map.lookup k <$> State.get
      case cacheEntry of
        Nothing -> do
          logDebug $ "Cache miss for: " <> show k
          pure Nothing
        Just (entryTime, v) -> do
          logDebug $ "Cache hit for: " <> show k
          now <- getTime
          if isValid strat entryTime now
            then pure $ Just v
            else do
              logDebug "Cache line to old"
              pure Nothing
    Update k v -> do
      now <- getTime
      State.modify (Map.insert k (now, v))
