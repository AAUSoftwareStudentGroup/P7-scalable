module Recommendation.MatchExecutor where

import Recommendation.Recommender (predict, match, defaultPredictionOptions)
import qualified Database as Db
import Data.Text (Text)
import qualified Transformer
import qualified Numeric.LinearAlgebra.Data as LAD
import Numeric.LinearAlgebra.Data (Matrix(..))
import Recommendation.DataLoad (loadMatrixFromFile)

type Username = Text

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
      let itemCount = length itemEmb
      let answerVector = Transformer.toAnswerVector itemCount userAnswers
      -- Predict the rest of the answers
      predictedAnswerVector <- predict defaultPredictionOptions answerVector (LAD.fromLists itemEmb)
      
      -- Save the predictions to the database
      Db.updatePredictedAnswers mongoInfo [(username, Transformer.fromAnswerVector answerVector)] 

      eitherCorrelationMatrix <- loadCorrelationMatrix
      case eitherCorrelationMatrix of
        Left err -> error $ "CorrelationMatrix could not be loaded: " ++ err
      matches <- undefined -- match user
      putStrLn "match"
      -- save Matches


loadCorrelationMatrix :: IO (Either String (Matrix Double))
loadCorrelationMatrix = loadMatrixFromFile "Data/correlations.csv"