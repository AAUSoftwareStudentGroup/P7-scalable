module Main where

import           API
import           Control.Monad              (void)
import qualified Numeric.LinearAlgebra.Data as LAD
import           Recommendation.DataLoad
import           Recommendation.Recommender
import qualified Database as Db
import Schema

main :: IO ()
main = predictTheFuture
  -- -- putStrLn "RUNNING SERVER" *> runServer

predictTheFuture :: IO ()
predictTheFuture = do
  putStrLn "PREDICTIONS FROM THE FUUUTUURE"
  answers <- loadMatrixFromFile "data/anonymous.csv"
  questionEmbedding <- Db.fetchMongoInfo >>= Db.fetchBestQuestionEmbedding
  case questionEmbedding of
    Nothing -> putStrLn "No question embedding found.."
    Just q -> case answers of
      Left err -> putStrLn err
      Right a  -> do
        result <- predict defaultOptions a (LAD.fromLists . questionEmbeddingEmbedding $ q)
        print result

startTraining :: IO ()
startTraining = do
  putStrLn "BEGINNING TO TRAIN"
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ train defaultOptions kValue a
  where
    kValue = 20
