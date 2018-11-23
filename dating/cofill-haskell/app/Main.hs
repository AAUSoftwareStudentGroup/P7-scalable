module Main where

import TestColFil (runEverything)
import DataTransformation (toAssocMatrix)
import DataLoad (loadRatings)
import Numeric.LinearAlgebra.Data (toDense)
import Numeric.LinearAlgebra.HMatrix (mul)
import CollaborativeFiltering (gradientDescent)

main :: IO ()
main = do 
  result <- loadRatings
  case result of
    Left err -> putStrLn err
    Right ratings -> 
        do
        let matrix = toDense . toAssocMatrix $ ratings
        let minMaxIter = (10, 50)
        let threshold  = 0.01
        let learnRate  = 0.0000001
        let kValue     = 7

        (u, q) <- gradientDescent minMaxIter threshold learnRate matrix kValue
        print $ mul u q