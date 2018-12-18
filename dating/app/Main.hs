module Main where

import           API
import           Control.Monad              (void)
import qualified Database                   as Db
import qualified Data.List as List
import           Schema
import           FrontendTypes
import           Recommendation.Recommender
import           Recommendation.DataLoad
import           Recommendation.MatchExecutor
import Data.Text

main :: IO ()
main = predictAll
  --mapM_ startTraining [5, 10, 15, 20, 30, 40, 50, 60, 80]
  --getAnswersForAllUsers


getAnswersForAllUsers :: IO ()
getAnswersForAllUsers = do
  mongoInfo <- Db.fetchMongoInfo
  users <- Db.fetchAllUsers mongoInfo
  mapM_ getAnsweredQuestions users

getAnsweredQuestions username = do
  mongoInfo <- Db.fetchMongoInfo
  userAnswers <- Db.fetchNonPredictedAnswers mongoInfo username
  let answers = List.length userAnswers
  if answers >= 10
  then print $ show username ++ ": " ++ show answers
  else putStr ""

testUserPredictions :: IO ()
testUserPredictions = do
  let user = "frækfyr3"
  putStrLn $ "FINDING PREDICTION ERROR FOR " ++ user
  error <- getPredictionErrorForUser $ pack user
  print error

predictAll :: IO ()
predictAll = do 
  putStrLn "BEGINNING TO PREDICT"
  matchAllUsers
  putStrLn "Done.."

startTraining :: Int -> IO ()
startTraining kValue = do
  putStrLn $ "TRAINING WITH K = " ++ show kValue
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ train defaultTrainingOptions kValue a
  

run :: IO ()
run = putStrLn "RUNNING SERVER" *> runServer
