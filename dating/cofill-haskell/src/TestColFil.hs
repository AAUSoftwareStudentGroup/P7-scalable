module TestColFil where

import           CollaborativeFiltering
import           DataLoad
import           Numeric.LinearAlgebra         (cmap, size, sumElements, tr',
                                                (><))
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Question
import           System.Random

trainAndGetQ :: Matrix -> IO (EmbeddingMatrix, Double)
trainAndGetQ answerMatrix = do
  (u, q) <- train minMaxIter threshold learnRate testMatrix kValue
  let guess = mul u q
  let mse = meanSquareError (guess - answerMatrix) mSize
  return (q, mse)

  where

    minMaxIter = (10, 50)
    threshold  = 0.01
    learnRate  = 0.0000001
    kValue     = 20
    (rows, cols) = size answerMatrix
    mSize        = fromIntegral $ rows * cols
    testMatrix   = getTestMatrix answerMatrix

    getTestMatrix :: Matrix -> Matrix
    getTestMatrix answerMatrix = do
        let (rows, cols)  = size answerMatrix
        oneZeroMatrix     <- mkRandomMatrix rows cols
        answerMatrix * oneZeroMatrix

    rndOneOrZero :: Matrix -> Matrix
    rndOneOrZero = cmap (\x -> if x > 0.7 then 0 else 1)

    mkMatrix :: Int -> Int -> [Double] -> Matrix
    mkMatrix rows cols lst = tr' $ (cols><rows) lst :: Matrix

    mkRandomMatrix :: Int -> Int -> IO Matrix
    mkRandomMatrix cols rows = rndOneOrZero . mkMatrix cols rows <$> getRandomNumbers

    convertQuestionsToList :: [Question] -> [Double]
    convertQuestionsToList =
        concatMap (toScores . survey_answers)

    toScores :: [SurveyAnswer] -> [Double]
    toScores = map score
