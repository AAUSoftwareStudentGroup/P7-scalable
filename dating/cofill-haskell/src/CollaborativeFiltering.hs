module CollaborativeFiltering(gradientDescent, mul, Matrix, getRandomNumbers, meanSquareError, toDense) where

import           Control.Monad                 (void)
import           Debug.Trace                   (trace)
import           Numeric.LinearAlgebra         (cmap, size, sumElements, tr',
                                                (><))
import qualified Numeric.LinearAlgebra         as LA
import           Numeric.LinearAlgebra.Data    (toDense, GMatrix)
import           Numeric.LinearAlgebra.HMatrix (mul)
import           System.Random                 (newStdGen, randomRs)
import Control.Arrow ((&&&))

toOneOrZero :: Matrix -> Matrix
toOneOrZero = cmap (\x -> if x == 0 then 0 else 1)

getRandomNumbers :: IO [Double]
getRandomNumbers = randomRs (0, 1) <$> newStdGen

type Matrix = LA.Matrix LA.R
type EmbeddingMatrix = Matrix
type LearningRate = Matrix
type MinMaxIterations = (Int, Int)

mkEmbeddinMatrix :: Int -> Int -> IO Matrix
mkEmbeddinMatrix rows cols = (rows><cols) <$> getRandomNumbers

gradientDescent :: MinMaxIterations -> Double -> LearningRate -> Matrix -> Int -> IO (Matrix, Matrix)
gradientDescent minMaxIter threshold learningRate answers kValue = do
    userEmbedding <- mkEmbeddinMatrix rows kValue
    questEmbedding <- mkEmbeddinMatrix kValue cols
    return $ gradientDescent' minMaxIter 1 threshold learningRate learningRate Nothing answers (toOneOrZero answers) userEmbedding questEmbedding
    where
        (rows, cols)   = size answers

gradientDescent' ::
    MinMaxIterations
    -> Int
    -> Double
    -> LearningRate
    -> LearningRate
    -> Maybe Double
    -> Matrix
    -> Matrix
    -> Matrix
    -> Matrix
    -> (Matrix, Matrix)
gradientDescent' iterRange iter threshold alpha alphaInitial preMse a aHasValue u q =
    if continue iterRange iter threshold mse preMse
    then gradientDescent' iterRange (iter+1) threshold newAlp alphaInitial (Just mse) a aHasValue u' q'
    else (u, q)
  where
    guess  = mul u q
    guess' = guess * aHasValue
    error  = guess' - a
    mse    = meanSquareError (a - guess') $ sumElements aHasValue
    u'     = u - tr' (mul q (tr' error)) * alpha
    q'     = q - mul (tr' u) error * alpha
    newAlp = if isSmaller mse preMse then alpha + alphaInitial else alphaInitial
    arrow  = if isSmaller mse preMse then " ▲  " else "  ▼ "
    debug  = arrow ++ show iter ++ ": MSE: " ++ show mse


data StochasticParameters = StochasticParameters
    { learningRate :: LearningRate
    , iterations   :: Integer
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


stochastic :: EmbeddingMatrix -> EmbeddingMatrix -> Matrix -> Matrix
stochastic = undefined

regular :: EmbeddingMatrix -> EmbeddingMatrix -> Matrix -> Matrix
regular users items expected = undefined
