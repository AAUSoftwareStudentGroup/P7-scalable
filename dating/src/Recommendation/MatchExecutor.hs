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


loadCorrelationMatrix :: IO (Either String (Matrix Double))
loadCorrelationMatrix = loadMatrixFromFile "Data/correlations.csv"