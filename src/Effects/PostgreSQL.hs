{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effects.PostgreSQL
  ( PostgreSQL (..),
    query_,
    query,
    execute_,
    execute,
    runPostgreSQL,
    Query,
    FromRow (..),
    ToRow (..),
    ToField (..),
    FromField (..),
    Only (..),
    SqlError,
    Error.Error,
    Resource,
  )
where

import Database.PostgreSQL.Simple (ConnectInfo, Connection, Only (..), Query, SqlError, close, connect)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int (Int64)
import Polysemy
import qualified Polysemy.Error as Error
import Polysemy.Resource

data PostgreSQL m r where
  Query :: (FromRow r, ToRow q) => Query -> q -> PostgreSQL m [r]
  Query_ :: FromRow r => Query -> PostgreSQL m [r]
  Execute :: ToRow q => Query -> q -> PostgreSQL m Int64
  Execute_ :: Query -> PostgreSQL m Int64

makeSem ''PostgreSQL

withConnection :: Members '[Embed IO] r => ConnectInfo -> (Connection -> Sem (Resource : r) a) -> Sem r a
withConnection connectInfo = resourceToIO . bracket (embed $ connect connectInfo) (embed . close)

runPostgreSQL :: Members '[Embed IO, Error.Error SqlError] r => ConnectInfo -> Sem (PostgreSQL : Resource : r) a -> Sem r a
runPostgreSQL connectInfo act =
  withConnection connectInfo $
    \conn ->
      interpret
        ( \case
            Query queryStr queryRow -> Error.fromException @SqlError $ DB.query conn queryStr queryRow
            Query_ queryStr -> Error.fromException @SqlError $ DB.query_ conn queryStr
            Execute queryStr queryRow -> Error.fromException @SqlError $ DB.execute conn queryStr queryRow
            Execute_ queryStr -> Error.fromException @SqlError $ DB.execute_ conn queryStr
        )
        act
