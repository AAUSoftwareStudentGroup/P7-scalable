module Main where

import TestColFil (runEverything)
import DataTransformation (toAssocMatrix)
import DataLoad (loadRatings)
import Numeric.LinearAlgebra.Data (toDense)

main :: IO ()
main = do 
  result <- loadRatings
  case result of
    Left err -> putStrLn err
    Right ratings -> print . toDense . toAssocMatrix $ ratings