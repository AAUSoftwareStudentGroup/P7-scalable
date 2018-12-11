module Main where

import           API
import           Control.Monad              (void)
import qualified Database                   as Db
import           Recommendation.Recommender
import qualified Data.List as List
import           Schema
import           FrontendTypes

main :: IO ()
main = putStrLn "RUNNING SERVER!" *> runServer-- SERVER" *> runServer
--main = putStrLn "RUNNING SERVER" *> runServer
