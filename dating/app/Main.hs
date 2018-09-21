module Main where

import Database (localConnString, migrateDB)
import API (runServer)

main :: IO ()
main = do
  putStrLn "TRYING TO MIGRATING DATABASE..."
  _ <- migrateDB localConnString
  putStrLn "SUCCESS"
  putStrLn "TRYING TO RUN SERVER..."
  runServer
