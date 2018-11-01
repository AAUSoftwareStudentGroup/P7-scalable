module Main where

import Database (createUser, localMongoConf)
-- import API (runServer)

main :: IO ()
main = do 
  --putStr "DELETING EVERYTHING IN DB..."
  --deleteEverythingInDB localConnString
  --putStrLn "DONE"
  putStr "Create user"
  _ <- createUser localMongoConf "ok"
  putStrLn "DONE"
  --putStrLn "GENERATING ELM CODE"
  --ElmCodeGen.genUsersApiCode
  --putStrLn "DONE"
  putStrLn "RUNNING SERVER"
  -- runServer
