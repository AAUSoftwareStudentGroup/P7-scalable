module Main where

import           API
import           Control.Monad              (void)
import qualified Numeric.LinearAlgebra.Data as LAD
import           Recommendation.DataLoad
import           Recommendation.Recommender

main :: IO ()
main = do
  putStrLn "BEGINNING TO TRAIN"
  answers <- loadMatrixFromFile "data/answers.csv"
  case answers of
    Left err -> putStrLn err
    Right a  -> void $ train defaultOptions kValue a

  where
    kValue = 20
  -- putStrLn "RUNNING SERVER" *> runServer
