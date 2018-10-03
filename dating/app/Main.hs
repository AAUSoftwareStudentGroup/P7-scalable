module Main where

import Database (localConnString, migrateDB)
import API (runServer)
import qualified ElmCodeGen (genUsersApiCode)

main :: IO ()
main = do 
  putStr "MIGRATING DB..."
  _ <- migrateDB localConnString
  putStrLn "DONE"
  putStrLn "GENERATING ELM CODE"
  ElmCodeGen.genUsersApiCode
  putStrLn "DONE"
  putStrLn "RUNNING SERVER"
  runServer
