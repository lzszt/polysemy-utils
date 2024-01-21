{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CacheSpec where

import Data.Map.Strict qualified as Map
import Data.Time
import Effects.Cache
import Effects.Logging
import Effects.Time
import Polysemy
import Polysemy.State qualified as State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils ()

-- spec :: Spec
-- spec = do
--   isValidSpec
--   cacheSpec

-- isValidSpec :: Spec
-- isValidSpec =
--   describe "isValid" $ do
--     prop "is always true for cache strategy Indefinitly" $ \cacheTime now ->
--       isValid Indefinitly cacheTime now `shouldBe` True
--     prop "is true if cache time is now" $ \strat now ->
--       isValid strat now now `shouldBe` True
--     prop "is true if time since cache is shorter than cache valid time" $ \(Positive cacheValidDiffTime) now cacheDiffTime ->
--       abs cacheDiffTime <= cacheValidDiffTime ==>
--         let strat = CacheFor cacheValidDiffTime
--             cacheTime = addUTCTime cacheDiffTime now
--          in isValid strat cacheTime now `shouldBe` True

-- cacheSpec :: Spec
-- cacheSpec = do
--   prop "An indefinitly cached value should always be cached" $
--     \k v cacheTime fixedTime ->
--       runCache @Int @Int fixedTime [(k, (cacheTime, v))] Indefinitly (get k)
--         `shouldBe` Just v
--   prop "get after update should always return the value from the update" $ \fixedTime initialState strat (k :: Int) (v :: Int) ->
--     runCache @Int @Int fixedTime initialState strat (update k v >> get k)
--       `shouldBe` Just v
--   prop "get after cache is expired results in Nothing" $
--     \k v cacheTime cacheValidTime accessTime ->
--       addUTCTime cacheValidTime cacheTime < accessTime ==>
--         let strat = CacheFor cacheValidTime
--             initialState = [(v, (cacheTime, k))]
--          in runCache @Int @Int accessTime initialState strat (get k)
--               `shouldBe` (Nothing @Int)

-- runCache ::
--   (Ord k, Show k) =>
--   UTCTime ->
--   [(k, (UTCTime, v))] ->
--   CacheStrategy s ->
--   Sem '[Cache k v, Time, Log] c ->
--   c
-- runCache fixedTime initialState strat =
--   run
--     . disableLogging
--     . runFixedTime fixedTime
--     . State.evalState (Map.fromList initialState)
-- . runCacheAsState strat
