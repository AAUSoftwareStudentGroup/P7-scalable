module Main where

import           API

main :: IO ()
main = putStrLn "RUNNING SERVER" *> runServer
