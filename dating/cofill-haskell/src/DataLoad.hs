module DataLoad (Rating(..), loadRatings) where

import Data.Csv
import Data.Vector (Vector)
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (mzero)

data Rating = Rating
    { userId  :: Int
    , movieId :: Int
    , rating  :: Double
    } deriving (Eq, Show)

instance FromRecord Rating where
  parseRecord v
      | length v == 4 = Rating <$> v .! 0 <*> v .! 1 <*> v .! 2
      | otherwise     = mzero

loadRatings :: String -> IO (Either String (Vector Rating))
loadRatings path = decode HasHeader <$> LBS.readFile path