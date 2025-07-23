module Main where
import Db (makePool, runMigrations)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(Proxy))

import Api (API)
import Server (appServer)
import Servant (serve)

main :: IO ()
main = do
  pool <- makePool
  runMigrations pool
  putStrLn "Running on :8000!"  
  run 8000 (serve (Proxy :: Proxy API) (appServer pool))
