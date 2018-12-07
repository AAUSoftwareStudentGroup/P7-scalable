module Transformer where

import qualified Numeric.LinearAlgebra.Data as LAD
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector, (//))

import FrontendTypes (AnswerWithIndexDTO(..))

type AnswerVector = Vector Double

toAnswerVector :: Int -> [AnswerWithIndexDTO] -> AnswerVector
toAnswerVector sizeOfVector answerDTOs = zeroVector // updatePairs
  where
    zeroVector = V.replicate sizeOfVector 0
    updatePairs = map (\dto -> (questionIndex dto, score dto)) answerDTOs

fromAnswerVector :: AnswerVector -> [AnswerWithIndexDTO]
fromAnswerVector av = zipWith (flip AnswerWithIndexDTO) (V.toList av) [0..]