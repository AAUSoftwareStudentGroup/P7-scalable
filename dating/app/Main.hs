{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Database
import API

-- import API (runServer)

main :: IO ()
main = do 
  --putStr "DELETING EVERYTHING IN DB..."
  --deleteEverythingInDB localConnString
  --putStrLn "DONE"
  -- putStr "Create user"
  -- let userDTO = CreateUserDTO "kasper@bargsteen" "repsak" "bargsteen" Male (fromGregorian 1994 05 06) "Aalborg" "Hej I big butts"
  -- loggedInDTO <- createUser localMongoInfo userDTO
  -- putStrLn "DONE"
  -- logIn <- maybeLogin localMongoInfo $ CredentialDTO "bargsteen" "repsak"
  -- print logIn
  --putStrLn "GENERATING ELM CODE"
  --ElmCodeGen.genUsersApiCode
  --putStrLn "DONE"
  putStrLn "RUNNING SERVER"
  runServer
