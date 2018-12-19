module Main where

import           API
import qualified Database                   as Db

import           Schema
import           FrontendTypes
import           Recommendation.MatchExecutor

main :: IO ()
main = putStrLn "RUNNING SERVER" *> runServer
  --predictAll
  --mapM_ startTraining [5, 10, 15, 20, 30, 40, 50, 60, 80]
  --getAnswersForAllUsers