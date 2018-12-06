module Main where

import           API
import           Control.Monad              (void)
import qualified Database                   as Db
import qualified Numeric.LinearAlgebra.Data as LAD
import Data.Text (Text, pack)
import           Recommendation.DataLoad
import           Recommendation.Recommender
import qualified Data.List as List
import           Schema
import           FrontendTypes

main :: IO ()
main = putStrLn "RUNNING SERVER" *> runServer
{-  mong <- Db.fetchMongoInfo
  questions <- Db.fetchNonPredictedAnswers mong (pack "user9")
  putStrLn $ show $ List.length questions
 mong <- Db.fetchMongoInfo
 let answer0 = AnswerDTO (pack "0") 2
 let answer1 = AnswerDTO (pack "1") 0
 let answer2 = AnswerDTO (pack "2") 2
 let pairs = [((pack "aaaaa"), [answer0, answer1, answer2])]
 Db.updatePredictedAnswers mong pairs-}
--main = putStrLn "RUNNING SERVER" *> runServer --startStochasticTraining
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

startStochasticTraining :: IO ()
startStochasticTraining = do
  putStrLn "BEGINNING STOCHASTIC TRAINING"
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ stochasticTrain defaultTrainingOptions kValue a
  where
    kValue = 7