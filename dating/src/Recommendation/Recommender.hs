{-# LANGUAGE OverloadedStrings #-}

module Recommendation.Recommender where

import           Control.Monad                 (void)
import           Data.Generics.Product         (getField)
import           Debug.Trace                   (trace)
import           GHC.Generics                  (Generic (..))
import           Numeric.LinearAlgebra         (cmap, size, sumElements, tr',
                                                (><))
import qualified Numeric.LinearAlgebra         as LA
import           Numeric.LinearAlgebra.HMatrix (mul)
import qualified System.Random                 as Rand

import qualified Database                      as Db
import           FrontendTypes                 (EmbeddingsDTO (..))


{------------------------------------------------------------------------------}
{-                                    TYPES                                   -}
{------------------------------------------------------------------------------}

type Vector = LA.Vector LA.R
type Matrix = LA.Matrix LA.R
type EmbeddingMatrix = Matrix
type LearningRate = Matrix
type IterationRange = (Int, Int)
type EmbeddingPair = (EmbeddingMatrix, EmbeddingMatrix)


type Username    = String
type UserTrainingMatrix = (Username, Vector)
type Score       = (Username, Double)


data Options = Options
    { iterationRange      :: IterationRange
    , threshold           :: Double
    , initialLearningRate :: LearningRate
    } deriving (Eq, Show)


{------------------------------------------------------------------------------}
{-                                  PREDICTION                                -}
{------------------------------------------------------------------------------}


predict :: Options -> Matrix -> EmbeddingMatrix -> IO Matrix
predict options target itemEmb = do
    initialUserEmb   <- mkEmbeddingMatrix answerRows kValue
    (userEmb, _) <- go (initialUserEmb, itemEmb) 0 (initialLearningRate options) Nothing
    return $ mul userEmb itemEmb

    where

        iterationRange'       = iterationRange options
        threshold'            = threshold options
        initialLearningRate'  = initialLearningRate options
        targetHasValueMatrix = toOneOrZero target

        (kValue, _)  = size itemEmb
        (answerRows, _) = size target


        go :: EmbeddingPair -> Int -> LearningRate -> Maybe Double -> IO EmbeddingPair
        go embeddingPair iterations learningRate prevMSE =
          if shouldContinue iterationRange' iterations threshold' mse prevMSE
          then go embeddingPair' (iterations + 1) learningRate' (Just mse)
          else return embeddingPair'

          where
            guess = toTraining . mkGuess $ embeddingPair
            error = toTraining $ getError guess target

            embeddingPair' = updateEmbeddings target True learningRate guess embeddingPair

            mse = calcMSE error (sumElements targetHasValueMatrix)

            toTraining :: Matrix -> Matrix
            toTraining = (* targetHasValueMatrix)

            learningRate' :: LearningRate
            learningRate' = if isSmaller mse prevMSE
                            then learningRate + initialLearningRate'
                            else initialLearningRate'


{------------------------------------------------------------------------------}
{-                                   TRAINING                                 -}
{------------------------------------------------------------------------------}

train :: Options -> Int -> Matrix -> IO EmbeddingPair
train options kValue target = do
  trainingMatrix <- getTrainingMatrix target
  userEmb <- mkEmbeddingMatrix rows kValue
  itemEmb <- mkEmbeddingMatrix kValue cols
  go trainingMatrix (userEmb, itemEmb) 0 initialLearningRate' Nothing

  where
    mSize :: Double
    mSize = fromIntegral . (uncurry (*)) . size $ target

    getTrainingMatrix :: Matrix -> IO Matrix
    getTrainingMatrix m = (m *) <$> mkRandomMatrix (size m)

    rndOneOrZero :: Matrix -> Matrix
    rndOneOrZero = cmap (\x -> if x > 0.7 then 0 else 1)

    mkMatrix :: (Int, Int) -> [Double] -> Matrix
    mkMatrix (rows, cols) lst = (rows><cols) lst

    mkRandomMatrix :: (Int, Int) -> IO Matrix
    mkRandomMatrix dimensions = rndOneOrZero . mkMatrix dimensions <$> getRandomNumbers

    (rows, cols)   = size target

    iterationRange'        = iterationRange options
    threshold'             = threshold options
    initialLearningRate'   = initialLearningRate options
    targetHasValueMatrix   = toOneOrZero target

    go :: Matrix -> EmbeddingPair -> Int -> LearningRate -> Maybe Double -> IO EmbeddingPair
    go trainingMatrix embeddingPair iterations learningRate prevMSE = maybeSaveToDb *>
      if shouldContinue iterationRange' iterations threshold' trainingMSE prevMSE
      then trace debugMsg $ go trainingMatrix embeddingPair' (iterations+1) learningRate' (Just trainingMSE)
      else return embeddingPair'

      where
        testGuess = mkGuess embeddingPair
        testError = getError testGuess trainingMatrix

        trainingGuess = toTraining testGuess
        trainingError = toTraining testError

        embeddingPair' = updateEmbeddings trainingMatrix False learningRate trainingGuess embeddingPair

        trainingMSE = calcMSE trainingError (sumElements target)
        testMSE = calcMSE testError (fromIntegral . cellCount $ target)

        toTraining :: Matrix -> Matrix
        toTraining = (* targetHasValueMatrix)

        debugMsg :: String
        debugMsg = arrow ++ show iterations ++ ": MSE: " ++ show trainingMSE

        arrow :: String
        arrow = if isSmaller trainingMSE prevMSE then " ▲  " else "  ▼ "


        learningRate' :: LearningRate
        learningRate' = if isSmaller trainingMSE prevMSE
                        then learningRate + initialLearningRate'
                        else initialLearningRate'


        maybeSaveToDb :: IO ()
        maybeSaveToDb =
          if iterations `mod` 100 /= 0
          then return ()
          else
            do
              mongoInfo <- Db.fetchMongoInfo
              let (userEmb', itemEmb') = embeddingPair'
              let dto = EmbeddingsDTO testMSE iterations (LA.toLists userEmb') (LA.toLists itemEmb')
              Db.createEmbeddings mongoInfo dto




{------------------------------------------------------------------------------}
{-                                   HELPERS                                  -}
{------------------------------------------------------------------------------}

updateEmbeddings :: Matrix -> Bool -> LearningRate -> Matrix -> EmbeddingPair -> EmbeddingPair
updateEmbeddings target isPredicting learningRate guess (userEmb, itemEmb) = (userEmb', itemEmb')
  where
    userEmb' :: EmbeddingMatrix
    userEmb' = userEmb - tr' (mul itemEmb (tr' error)) * learningRate

    itemEmb' :: EmbeddingMatrix
    itemEmb' = if isPredicting
               then itemEmb
               else itemEmb - mul (tr' userEmb) error * learningRate

    error :: Matrix
    error = guess - target


mkGuess :: EmbeddingPair -> Matrix
mkGuess = uncurry mul

getError :: Matrix -> Matrix -> Matrix
getError = (-)



calcMSE :: Matrix -> Double -> Double
calcMSE matrix noOfElements = mean (square matrix)
    where
        square :: Matrix -> Matrix
        square matrix = matrix * matrix

        mean :: Matrix -> Double
        mean matrix = sumElements matrix / noOfElements


getRandomNumbers :: IO [Double]
getRandomNumbers = Rand.randomRs (0, 1) <$> Rand.newStdGen


toOneOrZero :: Matrix -> Matrix
toOneOrZero = cmap (\x -> if x == 0 then 0 else 1)

mkEmbeddingMatrix :: Int -> Int -> IO Matrix
mkEmbeddingMatrix rows cols = (rows><cols) <$> getRandomNumbers


defaultPredictionOptions :: Options
defaultPredictionOptions =
  Options { iterationRange = (100, 101)
          , threshold = 0.001
          , initialLearningRate = 0.000001
          }


defaultTrainingOptions :: Options
defaultTrainingOptions =
  Options { iterationRange = (1000000, 10000000)
          , threshold = 0.001
          , initialLearningRate = 0.00001
          }

cellCount :: Matrix -> Int
cellCount = uncurry (*) . size


shouldContinue :: IterationRange -> Int -> Double -> Double -> Maybe Double -> Bool
shouldContinue _ _ _ _ Nothing = True
shouldContinue (minIterations, maxIterations) iter threshold newMse (Just oldMse) =
    (
        (threshold + newMse < oldMse || newMse > oldMse) -- continue if new mse is not improved enough or if it is worse
        && iter <= maxIterations                         -- as long as we are not above our max iteration limit
    )   || iter <= minIterations                         -- or continue if we have not yet reached our min iteration limit



isSmaller :: Double -> Maybe Double -> Bool
isSmaller _ Nothing  = False
isSmaller x (Just y) = x < y
