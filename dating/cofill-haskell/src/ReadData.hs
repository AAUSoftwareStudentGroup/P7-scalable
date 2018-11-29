{-# LANGUAGE OverloadedStrings #-}
module ReadData where

import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix

import Question (Question, SurveyAnswer)

jsonFile :: FilePath
jsonFile = "data-large.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getQuestions :: IO [Question]
getQuestions = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Question])
 case d of
  Left err -> return []
  Right ps -> return ps
