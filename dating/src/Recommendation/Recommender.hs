{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

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

import qualified Database as Db
import FrontendTypes (QuestionEmbeddingDTO(..))


{------------------------------------------------------------------------------}
{-                                    TYPES                                   -}
{------------------------------------------------------------------------------}

type Vector = LA.Vector LA.R
type Matrix = LA.Matrix LA.R
type EmbeddingMatrix = Matrix
type LearningRate = Matrix
type IterationRange = (Int, Int)


type Username    = String
type UserAnswers = (Username, Vector)
type Score       = (Username, Double)


data GradientDescentOptions = GradientDescentOptions
    { iterationRange      :: IterationRange
    , threshold           :: Double
    , initialLearningRate :: LearningRate
    , trainItemEmb        :: Bool
    , answerMatrix        :: Matrix
    , hasAnswerMatrix     :: Matrix
    } deriving (Eq, Show, Generic)

data Options = Options
    { iterationRange      :: IterationRange
    , threshold           :: Double
    , initialLearningRate :: LearningRate
    } deriving (Eq, Show, Generic)


{------------------------------------------------------------------------------}
{-                                  PREDICTION                                -}
{------------------------------------------------------------------------------}


predict :: Options -> Matrix -> EmbeddingMatrix -> IO Matrix
predict options answerMatrix itemEmbedding = do
    initialUserEmb   <- mkEmbeddingMatrix answerRows kValue
    (userEmbedding, _) <- gradientDescent gdOptions initialUserEmb itemEmbedding
    return $ mul userEmbedding itemEmbedding

    where

        (kValue, _)  = size itemEmbedding
        (answerRows, _) = size answerMatrix
        gdOptions = GradientDescentOptions
            { iterationRange   = getField @"iterationRange" options
            , threshold        = getField @"threshold" options
            , initialLearningRate = getField @"initialLearningRate" options
            , trainItemEmb     = False
            , answerMatrix     = answerMatrix
            , hasAnswerMatrix  = toOneOrZero answerMatrix }


{------------------------------------------------------------------------------}
{-                                   TRAINING                                 -}
{------------------------------------------------------------------------------}

train :: Options -> Int -> Matrix -> IO (EmbeddingMatrix, Double)
train options kValue answerMatrix = do
  testMatrix <- getTestMatrix answerMatrix
  (u, q) <- train' options kValue testMatrix
  let guess = mul u q
  let mse = meanSquareError (guess - answerMatrix) mSize
  return (q, mse)

  where
    mSize        = fromIntegral . (uncurry (*)) . size $ answerMatrix
    testMatrix   = getTestMatrix answerMatrix

    getTestMatrix :: Matrix -> IO Matrix
    getTestMatrix answerMatrix = do
        oneZeroMatrix     <- mkRandomMatrix (size answerMatrix)
        return $ answerMatrix * oneZeroMatrix

    rndOneOrZero :: Matrix -> Matrix
    rndOneOrZero = cmap (\x -> if x > 0.7 then 0 else 1)

    mkMatrix :: (Int, Int) -> [Double] -> Matrix
    mkMatrix (rows, cols) lst = (rows><cols) lst :: Matrix

    mkRandomMatrix :: (Int, Int) -> IO Matrix
    mkRandomMatrix dimensions = rndOneOrZero . mkMatrix dimensions <$> getRandomNumbers


    train' :: Options -> Int -> Matrix -> IO (EmbeddingMatrix, EmbeddingMatrix)
    train' options kValue answers = do
      userEmb <- mkEmbeddingMatrix rows kValue
      itemEmb <- mkEmbeddingMatrix kValue cols
      gradientDescent gdOptions userEmb itemEmb
      where
        (rows, cols)   = size answers
        gdOptions = GradientDescentOptions
            { iterationRange  = getField @"iterationRange" options
            , threshold        = getField @"threshold" options
            , initialLearningRate = getField @"initialLearningRate" options
            , trainItemEmb     = True
            , answerMatrix     = answers
            , hasAnswerMatrix  = toOneOrZero answers }



{------------------------------------------------------------------------------}
{-                              GRADIENT DESCENT                              -}
{------------------------------------------------------------------------------}

gradientDescent :: GradientDescentOptions -> EmbeddingMatrix -> EmbeddingMatrix -> IO (EmbeddingMatrix, EmbeddingMatrix)
gradientDescent options userEmb itemEmb = go options 1 initialLearningRate Nothing userEmb itemEmb
  where
    initialLearningRate = getField @"initialLearningRate" options
    iterationRange = getField @"iterationRange" options
    threshold = getField @"threshold" options

    go ::
        GradientDescentOptions
        -> Int
        -> LearningRate
        -> Maybe Double
        -> EmbeddingMatrix
        -> EmbeddingMatrix
        -> IO (EmbeddingMatrix, EmbeddingMatrix)
    go options iteration learningRate preMse userEmb itemEmb = saveToDb mse iteration itemEmb *>
        if continue iterationRange iteration threshold mse preMse
        then trace debug $ go options (iteration+1) newAlp (Just mse) userEmb' itemEmb'
        else return (userEmb, itemEmb)
      where
        guess     = mul userEmb itemEmb * hasAnswerMatrix options
        error     = guess - answerMatrix options
        mse       = meanSquareError (answerMatrix options - guess) $ sumElements $ hasAnswerMatrix options
        userEmb'  = userEmb - tr' (mul itemEmb (tr' error)) * learningRate
        itemEmb'  = if trainItemEmb options then itemEmb - mul (tr' userEmb) error * learningRate else itemEmb
        newAlp    = if isSmaller mse preMse then learningRate + initialLR else initialLR

        initialLR = getField @"initialLearningRate" options
        debug     = arrow ++ show iteration ++ ": MSE: " ++ show mse
        arrow     = if isSmaller mse preMse then " ▲  " else "  ▼ "

        isSmaller :: Double -> Maybe Double -> Bool
        isSmaller _ Nothing  = False
        isSmaller x (Just y) = x < y

        continue :: (Int, Int) -> Int -> Double -> Double -> Maybe Double -> Bool
        continue _ _ _ _ Nothing = True
        continue iterRange iter threshold newMse (Just oldMse) =
            (
                (threshold + newMse < oldMse || newMse > oldMse) -- continue if new mse is not improved enough or if it is worse
                && iter <= snd iterRange                         -- as long as we are not above our max iteration limit
            )   || iter <= fst iterRange                         -- or continue if we have not yet reached our min iteration limit


        saveToDb :: Double -> Int -> EmbeddingMatrix -> IO ()
        saveToDb mse' iterations' emb' = if iterations' `mod` 20 /= 0
                                         then return ()
                                         else
                                           do
                                             mongoInfo <- Db.fetchMongoInfo
                                             let dto = QuestionEmbeddingDTO mse' iterations' (LA.toLists emb')
                                             Db.createQuestionEmbedding mongoInfo dto

{------------------------------------------------------------------------------}
{-                                   HELPERS                                  -}
{------------------------------------------------------------------------------}

meanSquareError :: Matrix -> Double -> Double
meanSquareError matrix noOfElements = mean (square matrix)
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


defaultOptions :: Options
defaultOptions =
  Options { iterationRange = (9000000, 9000000)
          , threshold = 0.0001
          , initialLearningRate = 0.000001
          }
