{-# LANGUAGE ExplicitNamespaces #-}

module Server where
import Database.Persist.Postgresql (ConnectionPool, PersistEntity (Key))
import Api (API, TodoForm (TodoForm))
import Lucid (Html)
import View (renderHomePage, renderTodoItems)
import Model (fetchTodos, insertTodo, toggleTodo, Todo, deleteTodo)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant (Server, type (:<|>) ((:<|>)), Handler)


appServer :: ConnectionPool -> Server API
appServer pool = 
    getHomePage pool
    :<|> addTodoHandler pool
    :<|> toggleTodoHandler pool
    :<|> deleteTodoHandler pool


getHomePage :: ConnectionPool -> Handler( Html ())
getHomePage pool = do
    todos <- liftIO $ fetchTodos pool
    pure $ renderHomePage todos


addTodoHandler :: ConnectionPool -> TodoForm -> Handler( Html ())
addTodoHandler pool (TodoForm title)= do
   liftIO $ insertTodo pool title
   todos <- liftIO $ fetchTodos pool
   pure $ renderTodoItems todos

toggleTodoHandler :: ConnectionPool -> Key Todo ->Handler( Html ())
toggleTodoHandler pool tid = do
    liftIO $ toggleTodo pool tid
    todos <- liftIO $ fetchTodos pool
    pure $ renderTodoItems todos

deleteTodoHandler:: ConnectionPool -> Key Todo -> Handler( Html ())
deleteTodoHandler pool tid = do
    liftIO $ deleteTodo pool tid
    todos <- liftIO $ fetchTodos pool
    pure $ renderTodoItems todos
    
