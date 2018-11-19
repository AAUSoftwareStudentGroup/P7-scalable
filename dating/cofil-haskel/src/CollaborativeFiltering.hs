module CollaborativeFiltering where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix
import Debug.Trace (trace)
import System.Random (randomRs, newStdGen)
import Control.Monad (void)

fast_ai_answers = (15><15)
    [ 3  , 5  , 1  , 3  , 4, 4  , 5  , 2  , 5  , 5  , 4  , 5  , 5  , 2  , 5
    , 5  , 5  , 5  , 4  , 5, 4  , 4  , 5  , 4  , 4  , 5  , 5  , 3  , 4  , 5
    , 4  , 5  , 5  , 4  , 5, 3  , 4.5, 5  , 4.5, 5  , 5  , 5  , 4.5, 5  , 4
    , 5  , 4  , 4  , 3  , 5, 3  , 4  , 4.5, 4  , 0  , 3  , 3  , 5  , 3  , 0
    , 2.5, 0  , 2  , 5  , 0, 4  , 2.5, 0  , 5  , 5  , 3  , 3  , 4  , 3  , 2
    , 3  , 0  , 4  , 4  , 4, 3  , 0  , 3  , 4  , 4  , 4.5, 4  , 4.5, 4  , 0
    , 3  , 3  , 5  , 4.5, 5, 4.5, 2  , 4.5, 4  , 3  , 4.5, 4.5, 4  , 3  , 4
    , 5  , 5  , 5  , 4  , 0, 4  , 5  , 4  , 4  , 4  , 0  , 3  , 5  , 4  , 4
    , 4  , 5  , 4  , 5  , 4, 4  , 5  , 5  , 4  , 4  , 4  , 4  , 2  , 3.5, 5
    , 3  , 3.5, 3  , 2.5, 0, 0  , 3  , 3.5, 3.5, 3  , 3.5, 3  , 3  , 4  , 4
    , 5  , 5  , 4  , 3  , 5, 2  , 4  , 4  , 5  , 5  , 5  , 3  , 4.5, 3  , 4.5
    , 0  , 5  , 2  , 3  , 5, 0  , 5  , 5  , 0  , 2.5, 2  , 3.5, 3.5, 3.5, 5
    , 1  , 5  , 3  , 5  , 4, 5  , 5  , 0  , 2  , 5  , 5  , 3  , 3  , 4  , 5
    , 4.5, 4.5, 3.5, 3  , 4, 4.5, 4  , 4  , 4  , 4  , 3.5, 3  , 4.5, 4  , 4.5
    , 0  , 5  , 3  , 3  , 0, 3  , 5  , 0  , 5  , 5  , 5  , 5  , 2  , 5  , 4  ] :: Matrix R

group_answers :: Matrix R
group_answers = (6><10)
    [ 3, 4, 3, 1, 1, 5, 5, 2, 3, 3
    , 3, 2, 3, 1, 4, 5, 5, 3, 2, 1
    , 1, 1, 3, 1, 3, 1, 4, 3, 2, 2
    , 4, 1, 3, 1, 2, 4, 5, 3, 1, 2
    , 2, 4, 2, 1, 2, 5, 5, 2, 1, 1
    , 3, 4, 2, 1, 2, 5, 5, 4, 1, 1 ]

test_answers :: Matrix R
test_answers = (6><10)
    [ 0, 4, 3, 1, 1, 5, 5, 0, 3, 3
    , 3, 2, 3, 0, 4, 5, 5, 3, 2, 1
    , 1, 1, 3, 1, 3, 1, 4, 3, 0, 2
    , 4, 1, 3, 1, 2, 0, 5, 0, 1, 2
    , 2, 4, 0, 1, 2, 5, 0, 2, 1, 1
    , 3, 4, 2, 1, 2, 5, 5, 4, 0, 1 ]

toOneOrZero :: Matrix R -> Matrix R
toOneOrZero m = cmap (\x -> if x == 0 then 0 else 1) m


getRandomNumbers :: IO [R]
getRandomNumbers = do
    g <- newStdGen
    return $ randomRs (0.2, 0.8) g

type EmbeddingMatrix = Matrix R
type AnswerMatrix = Matrix R
type LearningRate = Matrix R

mkEmbeddinMatrix :: Int -> Int -> IO EmbeddingMatrix
mkEmbeddinMatrix rows cols = (rows><cols) <$> getRandomNumbers

alpha' :: LearningRate
alpha' = 0.001
threshold' = 0.01

runner :: IO R
runner = do
    guess <- (uncurry mul) <$> gradientDescent threshold' alpha' fast_ai_answers 5
    return $ meanSquareError (fast_ai_answers - guess)

--mse = meanSquareError(runner - fast_ai_answers)

gradientDescent :: Double -> LearningRate -> AnswerMatrix -> Int -> IO (EmbeddingMatrix, EmbeddingMatrix)
gradientDescent threshold learningRate answers kValue = do
    userEmbedding <- mkEmbeddinMatrix rows kValue
    questEmbedding <- mkEmbeddinMatrix kValue cols
    return $ gradientDescent' (500, 10000) 0 threshold learningRate Nothing answers (toOneOrZero answers) userEmbedding questEmbedding
    where
        (rows, cols)   = size answers
        

gradientDescent' :: (Int, Int) -> Int-> Double -> LearningRate -> Maybe Double -> AnswerMatrix -> AnswerMatrix -> EmbeddingMatrix -> EmbeddingMatrix -> (EmbeddingMatrix, EmbeddingMatrix)
gradientDescent' iterRange iter threshold alpha preMse a a' u q = 
    if continue iterRange iter threshold mse preMse
    then trace (show iter ++ ": MSE: " ++ show mse) $ gradientDescent' iterRange (iter+1) threshold alpha (Just mse) a a' u' q'
    else (u, q)
  where
    guess  = mul u q
    guess' = guess * a'
    error  = guess' - a
    mse   = (meanSquareError (a - guess'))
    
    u'     = u - (tr' $ mul q (tr' error)) * alpha
    q'     = q - (mul (tr' u) error) * alpha

continue :: (Int, Int) -> Int -> Double -> Double -> Maybe Double -> Bool
continue _ _ _ _ Nothing = True
continue iterRange iter threshold newMse (Just oldMse) =
    (
        (threshold + newMse < oldMse || newMse > oldMse) -- continue if new mse is not improved enough or if it worse
        && iter <= snd iterRange                         -- as long as we are not above our max iteration limit
    )   || iter <= fst iterRange                         -- or continue if we have not yet reached our min iteration limit
        

meanSquareError :: Matrix R -> R
meanSquareError matrix = mean (square matrix)
    where
        square :: Matrix R -> Matrix R
        square matrix = matrix * matrix

        mean :: Matrix R -> R
        mean matrix = (sumElements matrix) / matrixSize
            where 
                (rows, cols) = size matrix
                matrixSize = fromIntegral $ rows * cols