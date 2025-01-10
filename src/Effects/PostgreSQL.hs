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
  executeMany,
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
  -- | reexports from postgresql-simple
  DB.defaultConnectInfo,
  DB.ConnectInfo (..),
) where

import Control.Exception qualified as Ex
import Database.PostgreSQL.Simple (ConnectInfo, Connection, Only (..), Query, close, connect)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int (Int64)
import Polysemy
import Polysemy.Error qualified as Error
import Polysemy.Input
import Polysemy.Resource

data PostgreSQL m r where
  Query :: (FromRow r, ToRow q) => Query -> q -> PostgreSQL m [r]
  Query_ :: (FromRow r) => Query -> PostgreSQL m [r]
  Execute :: (ToRow q) => Query -> q -> PostgreSQL m Int64
  Execute_ :: Query -> PostgreSQL m Int64
  ExecuteMany :: (ToRow q) => Query -> [q] -> PostgreSQL m Int64

makeSem ''PostgreSQL

data PostgreSQLError
  = PSQLFormatError DB.FormatError
  | PSQLQueryError DB.QueryError
  | PSQLResultError DB.ResultError
  | PSQLSqlError DB.SqlError
  deriving (Show)

withConnection :: (Members '[Final IO] r) => ConnectInfo -> (Connection -> Sem (Resource : r) a) -> Sem r a
withConnection connectInfo = resourceToIOFinal . bracket (embedFinal $ connect connectInfo) (embedFinal . close)

runPostgreSQL :: (Members '[Final IO, Error.Error PostgreSQLError] r) => ConnectInfo -> Sem (PostgreSQL : Resource : r) a -> Sem r a
runPostgreSQL connectInfo act =
  withConnection connectInfo $
    \conn ->
      interpret
        ( \case
            Query queryStr queryRow -> postgreSQLErrorFromException $ DB.query conn queryStr queryRow
            Query_ queryStr -> postgreSQLErrorFromException $ DB.query_ conn queryStr
            Execute queryStr queryRow -> postgreSQLErrorFromException $ DB.execute conn queryStr queryRow
            Execute_ queryStr -> postgreSQLErrorFromException $ DB.execute_ conn queryStr
            ExecuteMany queryStr queryRows -> postgreSQLErrorFromException $ DB.executeMany conn queryStr queryRows
        )
        act

runPostgreSQLI :: (Members '[Final IO, Error.Error PostgreSQLError, Input Connection] r) => Sem (PostgreSQL : r) a -> Sem r a
runPostgreSQLI act = do
  conn <- input
  interpret
    ( \case
        Query queryStr queryRow -> postgreSQLErrorFromException $ DB.query conn queryStr queryRow
        Query_ queryStr -> postgreSQLErrorFromException $ DB.query_ conn queryStr
        Execute queryStr queryRow -> postgreSQLErrorFromException $ DB.execute conn queryStr queryRow
        Execute_ queryStr -> postgreSQLErrorFromException $ DB.execute_ conn queryStr
        ExecuteMany queryStr queryRows -> postgreSQLErrorFromException $ DB.executeMany conn queryStr queryRows
    )
    act

postgreSQLErrorFromException :: (Members '[Final IO, Error.Error PostgreSQLError] r) => IO a -> Sem r a
postgreSQLErrorFromException act =
  embedToFinal $
    Error.fromEitherM $
      Ex.catches
        (Right <$> act)
        [ Ex.Handler $ \ex -> pure $ Left $ PSQLFormatError ex
        , Ex.Handler $ \ex -> pure $ Left $ PSQLQueryError ex
        , Ex.Handler $ \ex -> pure $ Left $ PSQLResultError ex
        , Ex.Handler $ \ex -> pure $ Left $ PSQLSqlError ex
        ]
