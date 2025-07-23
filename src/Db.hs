{-# LANGUAGE OverloadedStrings #-}
module Db where

import Model (migrateAll)
import Database.Persist.Postgresql (ConnectionString, ConnectionPool, createPostgresqlPool, runMigration, runSqlPersistMPool)
import Control.Monad.Logger (runStderrLoggingT)

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack)

getConnStr :: IO ConnectionString
getConnStr = do
  env <- lookupEnv "DATABASE_URL"
  let conn = fromMaybe  "host=localhost dbname=tododb user=tobi password=postgres port=5432" env
  putStrLn $ "Connecting with: " ++ conn
  pure $ pack conn


makePool :: IO ConnectionPool
makePool = do
    connStr <- getConnStr
    runStderrLoggingT $ createPostgresqlPool connStr 5

runMigrations :: ConnectionPool-> IO ()
runMigrations pool = flip runSqlPersistMPool pool $ runMigration migrateAll