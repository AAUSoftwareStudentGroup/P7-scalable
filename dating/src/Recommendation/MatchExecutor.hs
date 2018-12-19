module Recommendation.MatchExecutor where


import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Numeric.LinearAlgebra         as LA
import qualified Numeric.LinearAlgebra.Data    as LAD
import           Numeric.LinearAlgebra.HMatrix (mul, (<.>))
import qualified Transformer

import           Control.Monad                 (void)
import qualified Data.List                     as List
import           Debug.Trace

import qualified Database                      as Db
import           Recommendation.DataLoad       (loadMatrixFromFile)
import           Recommendation.Recommender    (AnswerVector, Matrix,
                                                defaultPredictionOptions,
                                                defaultTrainingOptions,
                                                getPredictionError, match,
                                                predict, rndOneOrZero, train)

type Username = Text

getPredictionErrorForUser :: Username -> IO Double
getPredictionErrorForUser username = do
  -- Load item embedding from database
  mongoInfo <- Db.fetchMongoInfo
  maybeItemEmb <- Db.fetchBestItemEmbedding mongoInfo
  case maybeItemEmb of
    Nothing -> error "ItemEmbedding not found in database."
    Just itemEmb -> do
      -- Load actual answers from the user
      userAnswers <- Db.fetchNonPredictedAnswers mongoInfo username
      print (length userAnswers)

      let itemEmbMatrix = LAD.fromLists itemEmb
      let (_, itemCount) = LAD.size itemEmbMatrix
      let answerVector = Transformer.toAnswerVector itemCount userAnswers
      -- Predict the rest of the answers
      getPredictionError defaultPredictionOptions answerVector itemEmbMatrix


matchAllUsers :: IO ()
matchAllUsers = do
  mongoInfo <- Db.fetchMongoInfo
  users <- Db.fetchAllUsers mongoInfo
  mapM_ createMatchesForUser users


createMatchesForUser :: Username -> IO ()
createMatchesForUser username = do
  -- Load item embedding from database
  mongoInfo <- Db.fetchMongoInfo
  maybeItemEmb <- Db.fetchBestItemEmbedding mongoInfo
  case maybeItemEmb of
    Nothing -> error "ItemEmbedding not found in database."
    Just itemEmb -> do
      -- Load actual answers from the user
      userAnswers <- Db.fetchNonPredictedAnswers mongoInfo username
      let itemEmbMatrix = LAD.fromLists itemEmb
      let (_, itemCount) = LAD.size itemEmbMatrix
      let answerVector = Transformer.toAnswerVector itemCount userAnswers
      -- Predict the rest of the answers
      predictedAnswerVector <- predict defaultPredictionOptions answerVector itemEmbMatrix
      -- Save the predictions to the database
      Db.updatePredictedAnswers mongoInfo [(username, Transformer.fromAnswerVector predictedAnswerVector)]

      eitherCorrelationMatrix <- loadCorrelationMatrix
      case eitherCorrelationMatrix of
        Left err -> error $ "CorrelationMatrix could not be loaded: " ++ err
        Right correlationMatrix -> do

          -- Fetch the possible candidates for matching and transform them to the correct format
          userAnswerPairs <- Db.fetchOtherUsersAndAnswers mongoInfo username
          let candidates = fmap (Transformer.toAnswerVector itemCount) <$> userAnswerPairs

          -- Get matches and save them
          let matches = match correlationMatrix (username, predictedAnswerVector) candidates
          Db.saveMatchesToDb mongoInfo (toMatchingTriple <$> matches)
  where
    toMatchingTriple :: (Username, Double) -> (Username, Username, Double)
    toMatchingTriple (otherUsername, score) = (username, otherUsername, score)


loadCorrelationMatrix :: IO (Either String Matrix)
loadCorrelationMatrix = loadMatrixFromFile "data/correlations.csv"


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
  error <- getPredictionErrorForUser $ Text.pack user
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
