module Main where

import           API
import           Control.Monad              (void)
import qualified Database                   as Db
import           Recommendation.Recommender
import qualified Data.List as List
import           Schema
import           FrontendTypes
import           Recommendation.DataLoad

main :: IO ()
main = startTraining


startTraining :: IO ()
startTraining = do
  putStrLn "BEGINNING TO TRAIN"
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ train defaultTrainingOptions kValue a
  where
    kValue = 7

-- runServer-- SERVER" *> runServer
    
--main = putStrLn "RUNNING SERVER" *> runServer
