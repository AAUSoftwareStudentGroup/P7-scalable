module Main where

import           API
import           Control.Monad              (void)
import qualified Database                   as Db
import qualified Numeric.LinearAlgebra.Data as LAD
import           Recommendation.DataLoad
import           Recommendation.Recommender
import           Schema

main :: IO ()
main = startTraining
  -- -- putStrLn "RUNNING SERVER" *> runServer

predictTheFuture :: IO ()
predictTheFuture = do
  putStrLn "PREDICTIONS FROM THE FUUUTUURE"
  answers <- loadMatrixFromFile "data/anonymous.csv"
  embeddings <- Db.fetchMongoInfo >>= Db.fetchBestEmbeddings
  case embeddings of
    Nothing -> putStrLn "No question embedding found.."
    Just embs -> case answers of
      Left err -> putStrLn err
      Right ans  -> do
        result <- predict defaultPredictionOptions ans (LAD.fromLists . embeddingsItemEmb $ embs)
        print result

startTraining :: IO ()
startTraining = do
  putStrLn "BEGINNING TO TRAIN"
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ train defaultTrainingOptions kValue a
  where
    kValue = 7
