{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Request where

import Effects.Cache
import Polysemy

data Request x y r a where
  Request :: x -> Request x y r y

makeSem ''Request

runRequest ::
  (Members '[Embed m] r) =>
  (x -> m y) ->
  Sem (Request x y : r) a ->
  Sem r a
runRequest interpreterFn = interpret $ \case
  Request x -> embed $ interpreterFn x

cachedRequest ::
  (Members '[] r) =>
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
