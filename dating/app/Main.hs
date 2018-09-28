module Main where

import Database (localConnString, migrateDB)
import API (runServer)

main :: IO ()
main = do
  putStrLn "MIGRATING DB"
  _ <- migrateDB localConnString
  putStrLn "DONE"
  putStrLn "TRYING TO RUN SERVER..."
  runServer
