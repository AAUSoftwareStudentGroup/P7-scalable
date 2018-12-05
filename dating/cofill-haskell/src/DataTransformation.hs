{-# LANGUAGE RecordWildCards #-}
module DataTransformation where

import Data.Vector (Vector)
import Data.Vector as Vector
import DataLoad (Rating(..))
import Numeric.LinearAlgebra.Data (AssocMatrix)

toAssocMatrix :: Vector Rating -> AssocMatrix
toAssocMatrix = fmap fromRating . Vector.toList
  where
    fromRating :: Rating -> ((Int, Int), Double)
    fromRating Rating{..} = ((userId, movieId), rating)