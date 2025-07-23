{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Model where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (ConnectionPool, runSqlPersistMPool, PersistStoreWrite (insert_, replace, delete), Entity, selectList, PersistEntity (Key), get)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Todo
    description Text
    completed Bool
    created UTCTime default=now()
    deriving Show Eq
|]


insertTodo :: ConnectionPool -> Text -> IO ()
insertTodo pool t = do
    now <- getCurrentTime
    flip runSqlPersistMPool pool $ insert_ (Todo t False now)


fetchTodos :: ConnectionPool -> IO [Entity Todo]
fetchTodos pool = flip runSqlPersistMPool pool $ selectList [] []

toggleTodo :: ConnectionPool -> Key Todo ->IO ()
toggleTodo pool tid = flip runSqlPersistMPool pool $ do
    maybeTodo <- get tid
    case maybeTodo of
        Just (Todo t ts created) -> replace tid (Todo t (not ts) created)
        Nothing -> return ()


deleteTodo :: ConnectionPool -> Key Todo -> IO ()
deleteTodo pool = flip runSqlPersistMPool pool . delete 
