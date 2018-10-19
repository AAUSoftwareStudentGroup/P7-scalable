module Main where

import Database (localConnString, migrateDB, deleteEverythingInDB)
import API (runServer)

main :: IO ()
main = do 
  --putStr "DELETING EVERYTHING IN DB..."
  --deleteEverythingInDB localConnString
  --putStrLn "DONE"
  putStr "MIGRATING DB..."
  _ <- migrateDB localConnString
  putStrLn "DONE"
  --putStrLn "GENERATING ELM CODE"
  --ElmCodeGen.genUsersApiCode
  --putStrLn "DONE"
  putStrLn "RUNNING SERVER"
  runServer
