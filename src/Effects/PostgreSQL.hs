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

module Effects.PostgreSQL (
  PostgreSQL (..),
  query_,
  query,
  execute_,
  execute,
  runPostgreSQL,
  runPostgreSQLI,
  Query,
  FromRow (..),
  ToRow (..),
  ToField (..),
  FromField (..),
  Only (..),
  PostgreSQLError (..),
  Error.Error,
  Resource,
) where

import qualified Control.Exception as Ex
import Database.PostgreSQL.Simple (ConnectInfo, Connection, Only (..), Query, close, connect)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int (Int64)
import Polysemy
import qualified Polysemy.Error as Error
import Polysemy.Input
import Polysemy.Resource

data PostgreSQL m r where
  Query :: (FromRow r, ToRow q) => Query -> q -> PostgreSQL m [r]
  Query_ :: FromRow r => Query -> PostgreSQL m [r]
  Execute :: ToRow q => Query -> q -> PostgreSQL m Int64
  Execute_ :: Query -> PostgreSQL m Int64

makeSem ''PostgreSQL

data PostgreSQLError
  = PSQLFormatError DB.FormatError
  | PSQLQueryError DB.QueryError
  | PSQLResultError DB.ResultError
  | PSQLSqlError DB.SqlError
  deriving (Show)

withConnection :: Members '[Embed IO] r => ConnectInfo -> (Connection -> Sem (Resource : r) a) -> Sem r a
withConnection connectInfo = resourceToIO . bracket (embed $ connect connectInfo) (embed . close)

runPostgreSQL :: Members '[Embed IO, Error.Error PostgreSQLError] r => ConnectInfo -> Sem (PostgreSQL : Resource : r) a -> Sem r a
runPostgreSQL connectInfo act =
  withConnection connectInfo $
    \conn ->
      interpret
        ( \case
            Query queryStr queryRow -> postgreSQLErrorFromException $ DB.query conn queryStr queryRow
            Query_ queryStr -> postgreSQLErrorFromException $ DB.query_ conn queryStr
            Execute queryStr queryRow -> postgreSQLErrorFromException $ DB.execute conn queryStr queryRow
            Execute_ queryStr -> postgreSQLErrorFromException $ DB.execute_ conn queryStr
        )
        act

runPostgreSQLI :: Members '[Embed IO, Error.Error PostgreSQLError, Input Connection] r => Sem (PostgreSQL : r) a -> Sem r a
runPostgreSQLI act = do
  conn <- input
  interpret
    ( \case
        Query queryStr queryRow -> postgreSQLErrorFromException $ DB.query conn queryStr queryRow
        Query_ queryStr -> postgreSQLErrorFromException $ DB.query_ conn queryStr
        Execute queryStr queryRow -> postgreSQLErrorFromException $ DB.execute conn queryStr queryRow
        Execute_ queryStr -> postgreSQLErrorFromException $ DB.execute_ conn queryStr
    )
    act

postgreSQLErrorFromException :: Members '[Embed IO, Error.Error PostgreSQLError] r => IO a -> Sem r a
postgreSQLErrorFromException act =
  Error.fromEitherM $
    Ex.catches
      (Right <$> act)
      [ Ex.Handler $ \ex -> pure $ Left $ PSQLFormatError ex
      , Ex.Handler $ \ex -> pure $ Left $ PSQLQueryError ex
      , Ex.Handler $ \ex -> pure $ Left $ PSQLResultError ex
      , Ex.Handler $ \ex -> pure $ Left $ PSQLSqlError ex
      ]
