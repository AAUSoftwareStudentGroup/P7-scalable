{-# LANGUAGE RecordWildCards #-}
module CollaborativeFiltering(train, predict, mul, Matrix, getRandomNumbers, meanSquareError, toDense) where

import           Control.Monad                 (void)
import           Debug.Trace                   (trace)
import           Numeric.LinearAlgebra         (cmap, size, sumElements, tr',
                                                (><))
import qualified Numeric.LinearAlgebra         as LA
import           Numeric.LinearAlgebra.Data    (toDense, GMatrix)
import           Numeric.LinearAlgebra.HMatrix (mul)
import           System.Random                 (newStdGen, randomRs)
import           Control.Arrow ((&&&))
import           Data.List
toOneOrZero :: Matrix -> Matrix
toOneOrZero = cmap (\x -> if x == 0 then 0 else 1)

getRandomNumbers :: IO [Double]
getRandomNumbers = randomRs (0, 1) <$> newStdGen

type Vector = LA.Vector LA.R
type Matrix = LA.Matrix LA.R
type EmbeddingMatrix = Matrix
type LearningRate = Matrix
type MinMaxIterations = (Int, Int)


data GradientDescentOptions = GradientDescentOptions
    { iterationsRange   :: MinMaxIterations
    , threshold         :: Double
    , initialLearnRate  :: LearningRate
    , trainItemEmb      :: Bool
    , answerMatrix      :: Matrix
    , hasAnswerMatrix   :: Matrix
    }


mkEmbeddinMatrix :: Int -> Int -> IO Matrix
mkEmbeddinMatrix rows cols = (rows><cols) <$> getRandomNumbers

predict :: Matrix -> EmbeddingMatrix -> IO Matrix
predict answerMatrix itemEmb = do 
    initialUserEmb   <- mkEmbeddinMatrix 1 kValue
    let (userEmb, _) = gradientDescent options 1 learnRate Nothing initialUserEmb itemEmb
    return $ mul userEmb itemEmb
    where
        learnRate    = 0.0001
        (kValue, _)  = size itemEmb
        options      = GradientDescentOptions 
            { iterationsRange  = (1000,1000) 
            , threshold        = 1
            , initialLearnRate = learnRate
            , trainItemEmb     = False
            , answerMatrix     = answerMatrix
            , hasAnswerMatrix  = toOneOrZero answerMatrix } 


train :: MinMaxIterations -> Double -> LearningRate -> Matrix -> Int -> IO (EmbeddingMatrix, EmbeddingMatrix)
train minMaxIter threshold learningRate answers kValue = do
    userEmbedding <- mkEmbeddinMatrix rows kValue
    questEmbedding <- mkEmbeddinMatrix kValue cols
    return $ gradientDescent options 1 learningRate Nothing userEmbedding questEmbedding
    where
        (rows, cols)   = size answers
        options = GradientDescentOptions 
            { iterationsRange  = minMaxIter 
            , threshold        = threshold
            , initialLearnRate = learningRate
            , trainItemEmb     = True
            , answerMatrix     = answers
            , hasAnswerMatrix  = toOneOrZero answers } 


gradientDescent ::
    GradientDescentOptions 
    -> Int
    -> LearningRate
    -> Maybe Double
    -> EmbeddingMatrix
    -> EmbeddingMatrix
    -> (Matrix, Matrix)
gradientDescent options@GradientDescentOptions{..} iteration learningRate preMse userEmb itemEmb =
    if continue iterationsRange iteration threshold mse preMse
    then gradientDescent options (iteration+1) newAlp (Just mse) userEmb' itemEmb'
    else (userEmb, itemEmb)
  where
    guess     = mul userEmb itemEmb * hasAnswerMatrix
    error     = guess - answerMatrix
    mse       = meanSquareError (answerMatrix - guess) $ sumElements hasAnswerMatrix
    userEmb'  = userEmb - tr' (mul itemEmb (tr' error)) * learningRate
    itemEmb'  = if trainItemEmb then itemEmb - mul (tr' userEmb) error * learningRate else itemEmb
    newAlp    = if isSmaller mse preMse then learningRate + initialLearnRate else initialLearnRate
    arrow     = if isSmaller mse preMse then " ▲  " else "  ▼ "
    debug     = arrow ++ show iteration ++ ": MSE: " ++ show mse


data StochasticParameters = StochasticParameters
    { learningRate :: LearningRate
    , users        :: EmbeddingMatrix
    , items        :: EmbeddingMatrix
    , answers      :: Matrix }

stochasticGradientDescent :: StochasticParameters -> (EmbeddingMatrix, EmbeddingMatrix)
stochasticGradientDescent = users &&& items


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


meanSquareError :: Matrix -> Double -> Double
meanSquareError matrix noOfElements = mean (square matrix)
    where
        square :: Matrix -> Matrix
        square matrix = matrix * matrix

        mean :: Matrix -> Double
        mean matrix = sumElements matrix / noOfElements

type Username    = String
type UserAnswers = (Username, Vector)
type Score       = (Username, Double)

findScores :: UserAnswers -> [UserAnswers] -> Matrix -> [Score]
findScores user users correlation = sorted
    where 
        scores = map toScore users
        sorted = sortBy (\(_,a) (_,b) -> compare b a) scores
        targetUserAnswers = snd user 
        
        toScore :: UserAnswers -> Score 
        toScore = fmap (\score -> getCorrelation targetUserAnswers score correlation) 


getCorrelation :: Vector -> Vector -> Matrix -> Double
getCorrelation user1 user2 matrix = sumElements correlation
    where
        ansMat1     = toNByNMatrix user1
        ansMat2     = tr' $ toNByNMatrix user2
        difference  = abs $ ansMat1 - ansMat2
        correlation = -(difference - 2) * matrix / 2

--Converts [A, B, C] into [[A, A, A], [B, B, B], [C, C, C]]
toNByNMatrix :: Vector -> Matrix
toNByNMatrix v = LA.fromBlocks [cols]
    where
        amount = size v
        cols   = replicate amount (LA.asColumn v)
