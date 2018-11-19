{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Question where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import Numeric.LinearAlgebra.HMatrix

data Question = Question
    { text              :: Text -- Should maybe be strict
    , survey_answers    :: [SurveyAnswer]
    } deriving (Show, Generic)

instance FromJSON Question
instance ToJSON Question


data SurveyAnswer = SurveyAnswer
    { respondent_id  :: Integer
    , score          :: R
    } deriving (Show, Generic)

instance FromJSON SurveyAnswer
instance ToJSON SurveyAnswer