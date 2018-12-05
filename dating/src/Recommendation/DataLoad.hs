{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Recommendation.DataLoad (Cell(..), loadMatrixFromFile, loadQuestionData) where

import           Control.Monad        (mzero)
import           Data.Aeson

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromRecord, HasHeader (..), parseRecord,
                                       (.!))
import qualified Data.Csv             as Csv (decode)
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector as Vector
import Numeric.LinearAlgebra.Data (AssocMatrix(..), toDense, Matrix)

{------------------------------------------------------------------------------}
{-                                   FRIENDS                                  -}
{------------------------------------------------------------------------------}


data Question = Question
    { body    :: Text -- Should maybe be strict
    , answers :: [Answer]
    } deriving (Show)

data Answer = Answer
    { answererId :: Integer
    , score      :: Double
    } deriving (Show)

instance FromJSON Question where
  parseJSON (Object o) = Question <$> o .: "text" <*> o .: "survey_answers"

instance FromJSON Answer where
  parseJSON (Object o) = Answer <$> o .: "respondent_id" <*> o .: "score"


loadQuestionData :: IO (Either String [Question])
loadQuestionData = eitherDecode <$> B.readFile "data/data-small.json"


{------------------------------------------------------------------------------}
{-                                 CSV LOADING                                -}
{------------------------------------------------------------------------------}

data Cell = Cell
    { rowId    :: Int
    , columnId :: Int
    , value    :: Double
    } deriving (Eq, Show)

instance FromRecord Cell where
  parseRecord v
      | length v == 3 = Cell <$> v .! 0 <*> v .! 1 <*> v .! 2
      | otherwise     = mzero

loadMatrixFromFile :: FilePath -> IO (Either String (Matrix Double))
loadMatrixFromFile path = fmap (toDense . toAssocMatrix) <$> loadFile
  where
    loadFile :: IO (Either String (Vector Cell))
    loadFile = Csv.decode NoHeader <$> LBS.readFile path

    toAssocMatrix :: Vector Cell -> AssocMatrix
    toAssocMatrix = fmap fromCell . Vector.toList
      where
        fromCell :: Cell -> ((Int, Int), Double)
        fromCell Cell{..} = ((rowId, columnId), value)
