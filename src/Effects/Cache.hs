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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Function
import Data.List
import Data.Map.Strict qualified as Map
import Data.Time
import Effects.Logging
import Effects.Time
import Polysemy
import Polysemy.State qualified as State

data Cache k v r a where
  Get :: k -> Cache k v r (Maybe v)
  Update :: k -> v -> Cache k v r ()

makeSem ''Cache

data Indef

data Fixed rs

data Finite

type family CacheContent s k v where
  CacheContent Finite k v = (UTCTime, v)
  CacheContent Indef k v = v
  CacheContent (Fixed rs) k v = (k, ReplacementStrategyInfo rs, v)

type family CacheType s k v where
  CacheType Finite k v = Map.Map k (CacheContent Finite k v)
  CacheType Indef k v = Map.Map k (CacheContent Indef k v)
  CacheType (Fixed rs) k v = [CacheContent (Fixed rs) k v]

data Oldest

data LeastUsed

type family ReplacementStrategyInfo rs where
  ReplacementStrategyInfo Oldest = UTCTime
  ReplacementStrategyInfo LeastUsed = Int

data ReplacementStrategy s where
  Oldest :: ReplacementStrategy Oldest
  LeastUsed :: ReplacementStrategy LeastUsed

data CacheStrategy s where
  Indefinitly :: CacheStrategy Indef
  FixedSize :: Int -> ReplacementStrategy rs -> CacheStrategy (Fixed rs)
  CacheFor :: NominalDiffTime -> CacheStrategy Finite

isValid :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
isValid diff cacheTime now = diffUTCTime now cacheTime <= diff

runCacheAsState ::
  (Members '[Time, Log] r) =>
  (Ord k) =>
  (Show k) =>
  CacheStrategy s ->
  Sem (Cache k v : r) a ->
  Sem (State.State (CacheType s k v) : r) a
runCacheAsState = \case
  Indefinitly ->
    reinterpret $ \case
      Get k -> do
        cacheEntry <- Map.lookup k <$> State.get
        case cacheEntry of
          Nothing -> do
            logDebug $ "Cache miss for: " <> show k
            pure Nothing
          Just v -> do
            logDebug $ "Cache hit for: " <> show k
            pure $ Just v
      Update k v -> do
        logDebug $ "Updated cache for: " <> show k
        State.modify' (Map.insert k v)
  FixedSize cacheSize rs ->
    reinterpret $ \case
      Get k -> do
        cacheEntry <- find (\(k', _, _) -> k' == k) <$> State.get
        case cacheEntry of
          Nothing -> do
            logDebug $ "Cache miss for: " <> show k
            pure Nothing
          Just (_, _, v) -> do
            logDebug $ "Cache hit for: " <> show k
            pure $ Just v
      Update k v -> do
        newInfo <- fixedInitialInfo rs
        cacheResidency <- State.gets length
        if cacheResidency == cacheSize
          then do
            logDebug $ "Updated cache for: " <> show k
            State.modify' (((k, newInfo, v) :) . removeByReplacementStrategy rs)
          else do
            logDebug $ "Updated cache for: " <> show k
            State.modify' ((k, newInfo, v) :)
  CacheFor cacheDuration ->
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
            if isValid cacheDuration entryTime now
              then pure $ Just v
              else do
                logDebug "Cache line to old"
                State.modify' (Map.delete k)
                logDebug $ "Remove from cache: " <> show k
                pure Nothing
      Update k v -> do
        now <- getTime
        logDebug $ "Updated cache for: " <> show k
        State.modify' (Map.insert k (now, v))

findKey :: (Ord k) => CacheStrategy s -> k -> v -> CacheType s k v -> Maybe (CacheContent s k v)
findKey cs k _ = case cs of
  Indefinitly -> Map.lookup k
  FixedSize _ _ -> find (\(k', _, _) -> k' == k)
  CacheFor _ -> Map.lookup k

fixedInitialInfo :: (Members '[Time] r) => ReplacementStrategy rs -> Sem r (ReplacementStrategyInfo rs)
fixedInitialInfo = \case
  Oldest -> getTime
  LeastUsed -> pure 0

removeByReplacementStrategy ::
  ReplacementStrategy rs ->
  [(a, ReplacementStrategyInfo rs, c)] ->
  [(a, ReplacementStrategyInfo rs, c)]
removeByReplacementStrategy = \case
  Oldest -> removeOldest
  LeastUsed -> removeLeastUsed
  where
    removeOldest xs = go xs
      where
        go (y@(_, t, _) : ys)
          | t == oldestTime = ys
          | otherwise = y : go ys
        go [] = error "Cannot remove element from empty cache"
        (_, oldestTime, _) = minimumBy (compare `on` (\(_, t, _) -> t)) xs

    removeLeastUsed xs = go xs
      where
        go (y@(_, t, _) : ys)
          | t == lowestUsage = ys
          | otherwise = y : go ys
        go [] = error "Cannot remove element from empty cache"
        (_, lowestUsage, _) = minimumBy (compare `on` (\(_, t, _) -> t)) xs
