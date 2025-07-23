module Main where
import Db (makePool, runMigrations)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(Proxy))

import Api (API)
import Server (appServer)
import Servant (serve)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import System.Environment (lookupEnv)

main :: IO ()
main = do
  maybePortStr <- lookupEnv "PORT"
  let port = fromMaybe 8000 (maybePortStr >>= readMaybe)
  pool <- makePool
  runMigrations pool
  putStrLn $ "Running on port: " ++ show port
  run port (serve (Proxy :: Proxy API) (appServer pool))