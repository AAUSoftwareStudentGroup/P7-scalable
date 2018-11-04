{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Database
import SchemaEnums (Gender(..))
import Data.Time.Calendar (fromGregorian)
import FrontendTypes
import Data.Text (Text)
import Data.Aeson
-- import API (runServer)

main :: IO ()
main = do 
  --putStr "DELETING EVERYTHING IN DB..."
  --deleteEverythingInDB localConnString
  --putStrLn "DONE"
  -- putStr "Create user"
  -- let userDTO = CreateUserDTO "kasper@bargsteen" "repsak" "bargsteen" Male (fromGregorian 1994 05 06) "Aalborg" "Hej I big butts"
  -- loggedInDTO <- createUser localMongoConf userDTO
  -- putStrLn "DONE"
  -- logIn <- maybeLogin localMongoConf $ CredentialDTO "bargsteen" "repsak"
  -- print logIn
  --putStrLn "GENERATING ELM CODE"
  --ElmCodeGen.genUsersApiCode
  --putStrLn "DONE"
  putStrLn "RUNNING SERVER"
  -- runServer
