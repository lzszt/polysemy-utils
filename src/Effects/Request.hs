{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Request where

import Data.Maybe
import Data.Time
import Effects.Cache
import Effects.Delay
import Effects.Logging
import Effects.Time
import Polysemy
import Polysemy.State qualified as State

data Request x y r a where
  Request :: x -> Request x y r y

makeSem ''Request

embedRequest ::
  (Members '[Embed m] r) =>
  (x -> m y) ->
  Sem (Request x y : r) a ->
  Sem r a
embedRequest interpreterFn = interpret $
  \(Request x) -> embed $ interpreterFn x

runRequest ::
  (x -> Sem r y) ->
  Sem (Request x y : r) a ->
  Sem r a
runRequest interpreterFn = interpret $
  \(Request x) -> interpreterFn x

cachedRequest ::
  Sem (Request x y : r) a ->
  Sem (Cache x y : Request x y : r) a
cachedRequest =
  reinterpret2 $ \case
    Request x -> do
      cachedY <- get x
      case cachedY of
        Just y -> pure y
        Nothing -> do
          y <- request x
          update x y
          pure y

data RateSpec a = RateSpec
  { interval :: NominalDiffTime,
    numberRequests :: a
  }

safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum = go Nothing
  where
    go acc [] = acc
    go Nothing (x : xs) = go (Just x) xs
    go acc@(Just y) (x : xs)
      | x > y = go (Just x) xs
      | otherwise = go acc xs

data RequestInfo a = RequestInfo
  { requestTime :: UTCTime,
    requestData :: a
  }

requestDelay ::
  ( Members
      '[ State.State [RequestInfo x],
         Time,
         Log
       ]
      r
  ) =>
  (Num l) =>
  (Ord l) =>
  [RateSpec l] ->
  (x -> l) ->
  l ->
  Sem r (Maybe NominalDiffTime)
requestDelay rateSpecs requestCost nextRequestCost = do
  now <- getTime
  requests <- State.get
  pure $
    safeMaximum $
      mapMaybe
        ( \RateSpec {..} ->
            let intervalStart = addUTCTime (negate interval) now
                requestsInInterval = filter ((>= intervalStart) . requestTime) requests
                earliestRequestInInterval = minimum $ map requestTime requestsInInterval
                requestsInIntervalCost = nextRequestCost + sum (map (requestCost . requestData) requestsInInterval)
             in if requestsInIntervalCost < numberRequests
                  then Nothing
                  else Just $ diffUTCTime earliestRequestInInterval intervalStart
        )
        rateSpecs

rateLimitedRequest' ::
  ( Members
      '[ State.State [RequestInfo x],
         Time,
         Delay,
         Log
       ]
      r
  ) =>
  (Num l) =>
  (Ord l) =>
  [RateSpec l] ->
  (x -> l) ->
  Sem (Request x y : r) a ->
  Sem (Request x y : r) a
rateLimitedRequest' rateSpecs requestCost =
  let longestInterval = maximum $ map interval rateSpecs
   in reinterpret $ \case
        Request x -> do
          now <- getTime
          potentialRequestDelay <- requestDelay rateSpecs requestCost (requestCost x)
          logDebug $ "Rate limiting delay: " <> show potentialRequestDelay
          case potentialRequestDelay of
            Nothing -> do
              let startOfLongestInterval = addUTCTime (negate longestInterval) now
              State.modify' ((RequestInfo now x :) . filter ((>= startOfLongestInterval) . requestTime))
              request x
            Just d -> do
              delay d
              delayedNow <- getTime
              let startOfLongestInterval = addUTCTime (negate longestInterval) delayedNow
              State.modify' ((RequestInfo delayedNow x :) . filter ((>= startOfLongestInterval) . requestTime))
              request x

rateLimitedRequest ::
  ( Members
      '[ State.State [RequestInfo x],
         Time,
         Delay,
         Log
       ]
      r
  ) =>
  (Num l) =>
  (Ord l) =>
  [RateSpec l] ->
  Sem (Request x y : r) a ->
  Sem (Request x y : r) a
rateLimitedRequest rateSpecs = rateLimitedRequest' rateSpecs (const 1)
