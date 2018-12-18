{-# LANGUAGE OverloadedStrings #-}

module Recommendation.Recommender (getPredictionError, snapExtremeValues, mkEmbeddingMatrix, match, predict, train, stochasticTrain, rndOneOrZero, defaultPredictionOptions, defaultTrainingOptions, fromDense, toDense, Matrix, AnswerVector, Vector) where

import           Control.Monad                 (void, when)
import           Data.Generics.Product         (getField)
import           Data.List                     (sortBy)
import           Data.Ord                      (Down (..), comparing)
import           Data.Text                     (Text)
import           Debug.Trace                   (trace)
import           GHC.Generics                  (Generic (..))
import           Numeric.LinearAlgebra         (cmap, size, sumElements, tr',
                                                (><))
import qualified Numeric.LinearAlgebra         as LA
import           Numeric.LinearAlgebra.Data    (AssocMatrix, toColumns, toList,
                                                (!), toDense)
import           Numeric.LinearAlgebra.HMatrix (mul, (<.>))
import qualified System.Random                 as Rand

import qualified Database                      as Db
import           FrontendTypes                 (EmbeddingsDTO (..))


{------------------------------------------------------------------------------}
{-                                    TYPES                                   -}
{------------------------------------------------------------------------------}

type Vector             = LA.Vector LA.R
type Matrix             = LA.Matrix LA.R

type EmbeddingMatrix    = Matrix
type EmbeddingPair      = (EmbeddingMatrix, EmbeddingMatrix)

type LearningRate       = Matrix
type AnswerVector       = Vector
type CorrelationMatrix  = Matrix

type IterationRange     = (Int, Int)
type Username           = Text


data Options = Options
    { iterationRange      :: IterationRange
    , threshold           :: Double
    , initialLearningRate :: LearningRate
    } deriving (Eq, Show)

{------------------------------------------------------------------------------}
{-                                   MATCHING                                 -}
{------------------------------------------------------------------------------}

match :: CorrelationMatrix -> (Username, AnswerVector) -> [(Username, AnswerVector)] -> [(Username, Double)]
match correlationMatrix (user, userAnswers) otherUsersAndAnswers = sortedMatches
  where
    sortedMatches :: [(Username, Double)]
    sortedMatches = sortBySndDesc $ zip usernames scores

    usernames = map fst otherUsersAndAnswers
    scores = map (toPercentage . calcScore . snd) otherUsersAndAnswers

    userAnswerMatrix = toNByNMatrix userAnswers

    -- 1825 is the magic maximum possible score
    toPercentage :: Double -> Double
    toPercentage val 
      | val > maxScore  = 100
      | val < -maxScore = 0 
      | otherwise       = 100 * (sin radian + 1) / 2
      where
        maxScore = 1825
        x        = pi/(maxScore*2)
        radian   = x*val

    calcScore :: Vector -> Double
    calcScore otherUser = sumElements correlation
      where
        otherUserAnswerMatrix = tr' $ toNByNMatrix otherUser
        difference            = abs $ userAnswerMatrix - otherUserAnswerMatrix
        correlation           = -(difference - 2) * correlationMatrix

    toNByNMatrix :: Vector -> Matrix
    toNByNMatrix v = LA.fromBlocks [cols]
      where
        amount = size v
        cols   = replicate amount (LA.asColumn v)

    sortBySndDesc :: Ord b => [(a, b)] -> [(a, b)]
    sortBySndDesc = sortBy (comparing $ Down . snd)

{------------------------------------------------------------------------------}
{-                                  PREDICTION                                -}
{------------------------------------------------------------------------------}


getPredictionError :: Options -> AnswerVector -> EmbeddingMatrix -> IO Double
getPredictionError options target itemEmb = do
  testMatrix <- getTrainingMatrix targetMatrix
  let testMatrixHasValue = toOneOrZero testMatrix
  let predictErrorHasValue = targetHasValue - testMatrixHasValue
  let testVector = toVector testMatrix
  prediction <- predict options testVector itemEmb
  let error = (LA.asRow prediction - LA.asRow target) * predictErrorHasValue
  let knownAnwers = sumElements predictErrorHasValue
  
  return $ calcMSE error knownAnwers
  where
    targetMatrix = LA.asRow target
    targetHasValue = toOneOrZero targetMatrix


predict :: Options -> AnswerVector -> EmbeddingMatrix -> IO AnswerVector
predict options target itemEmb = do
    initialUserEmb   <- mkEmbeddingMatrix answerRows kValue
    (userEmb, _) <- go (initialUserEmb, itemEmb) 0 (initialLearningRate options) Nothing
    return . toVector . snapExtremeValues $ mul userEmb itemEmb

    where
        target' = LA.asRow target

        iterationRange'       = iterationRange options
        threshold'            = threshold options
        initialLearningRate'  = initialLearningRate options
        targetHasValueMatrix  = toOneOrZero target'

        (kValue, _)  = size itemEmb
        (answerRows, _) = size target'


        go :: EmbeddingPair -> Int -> LearningRate -> Maybe Double -> IO EmbeddingPair
        go embeddingPair iterations learningRate prevMSE =
          print mse *>
          if shouldContinue iterationRange' iterations threshold' mse prevMSE
          then go embeddingPair' (iterations + 1) learningRate' (Just mse)
          else return embeddingPair'

          where
            guess = toTraining . mkGuess $ embeddingPair
            error = toTraining $ getError guess target'

            embeddingPair' = updateEmbeddings target' True learningRate guess embeddingPair

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

train :: Options -> Int -> Matrix -> IO ()
train options kValue target = do
  trainingMatrix <- getTrainingMatrix target
  userEmb <- mkEmbeddingMatrix rows kValue
  itemEmb <- mkEmbeddingMatrix kValue cols
  let trainingHasValueMatrix = toOneOrZero trainingMatrix
  let testValuesMatrix       = targetHasValueMatrix - trainingHasValueMatrix
  print $ sumElements testValuesMatrix
  go trainingMatrix (userEmb, itemEmb) 0 initialLearningRate' Nothing trainingHasValueMatrix testValuesMatrix

  where
    mSize :: Double
    mSize = fromIntegral . uncurry (*) . size $ target

    (rows, cols)   = size target

    iterationRange'        = iterationRange options
    threshold'             = threshold options
    initialLearningRate'   = initialLearningRate options
    targetHasValueMatrix   = toOneOrZero target
    

    go :: Matrix -> EmbeddingPair -> Int -> LearningRate -> Maybe Double -> Matrix -> Matrix -> IO ()
    go trainingMatrix embeddingPair iterations learningRate prevMSE trainingHasValueMatrix testValuesMatrix =
      if iterations == 200 * kValue
        then do
          saveToDb kValue iterations testMSE embeddingPair
          putStrLn $ "Training_MSE: " ++ show trainingMSE
          putStrLn $ "Test_MSE: " ++ show testMSE
        else go trainingMatrix embeddingPair' (iterations+1) learningRate' (Just trainingMSE) trainingHasValueMatrix testValuesMatrix
      
      where
        guess = mkGuess embeddingPair
        error = getError guess trainingMatrix

        trainingGuess = toTraining guess
        trainingError = toTraining error

        testGuess = toTest . snapExtremeValues $ guess
        testError = toTest $ getError testGuess target

        embeddingPair' = updateEmbeddings trainingMatrix False learningRate trainingGuess embeddingPair

        trainingMSE = calcMSE trainingError (sumElements trainingHasValueMatrix)
        testMSE = calcMSE testError (sumElements testValuesMatrix)

        toTraining :: Matrix -> Matrix
        toTraining = (* trainingHasValueMatrix)

        toTest :: Matrix -> Matrix
        toTest = (* testValuesMatrix)

        debugMsg :: String
        debugMsg = arrow ++ show iterations ++ ": MSE: " ++ show trainingMSE ++ " LR: " ++ show (learningRate' ! 0 ! 0) ++ " K: " ++ show kValue

        arrow :: String
        arrow = if isSmaller trainingMSE prevMSE then " ▲  " else "  ▼ "

        learningRate' :: LearningRate
        learningRate' = if isSmaller trainingMSE prevMSE
                        then learningRate + initialLearningRate'
                        else maxLearningRate initialLearningRate' (learningRate - initialLearningRate' * 4)

        maxLearningRate :: LearningRate -> LearningRate -> LearningRate
        maxLearningRate a b = if sumElements a >= sumElements b then a else b


stochasticTrain :: Options -> Int -> Matrix -> IO EmbeddingPair
stochasticTrain options kValue target = do
  trainingMatrix <- getTrainingMatrix target
  let targetAssoc = fromDense trainingMatrix
  print . length $ targetAssoc
  userEmb <- mkEmbeddingMatrix rows kValue
  itemEmb <- mkEmbeddingMatrix kValue cols
  print . size $ userEmb
  print . size $ itemEmb
  go targetAssoc 0 (userEmb, itemEmb)
  where
    learningRate  = initialLearningRate options
    (rows, cols) = size target
    elementsCount = fromIntegral . cellCount $ target

    go :: AssocMatrix -> Int -> EmbeddingPair -> IO EmbeddingPair
    go targetAssoc iterations embeddingPair = do
      error <- getErrorStochastic embeddingPair targetAssoc
      putStrLn $ "Error:" <> show error
      let embeddingPair' = updateEmbeddingsStochastic error learningRate embeddingPair
      if iterations <= snd (iterationRange options)
      then
        -- when (iterations `mod` 200 == 0 && iterations /= 0)
        --   (trace debugMsg (maybeSaveToDb kValue iterations mse embeddingPair))
          trace debugMsg go targetAssoc (iterations + 1) embeddingPair'
      else return embeddingPair'

      where
        mse = calcMSE target elementsCount
        debugMsg = show iterations ++ " MSE: " ++ show mse

    updateEmbeddingsStochastic :: (Matrix, Matrix) -> LearningRate -> EmbeddingPair -> EmbeddingPair
    updateEmbeddingsStochastic (userError, itemError) learningRate (userEmb, itemEmb) = (userEmb', itemEmb')
      where
        userEmb' :: EmbeddingMatrix
        userEmb' = userEmb - (learningRate * userError)

        itemEmb' :: EmbeddingMatrix
        itemEmb' = tr' (tr' itemEmb - (learningRate * itemError))

    getErrorStochastic :: EmbeddingPair -> AssocMatrix -> IO (Matrix, Matrix)
    getErrorStochastic (userEmb, itemEmb) target = do
      ((row, column), value) <- getRandomFrom target
      let valueAsMatrix = (1><1) [value]
      let error = getError (guess row column) valueAsMatrix
      let itemCol = LA.row . toList $ (toColumns itemEmb !! column)
      let userRow = LA.row . toList $ (userEmb ! row)
      return (error * itemCol, error * userRow) -- We multiply with the other part to remove their influence
      where
        guess :: Int -> Int -> Matrix
        guess row col = (1><1) [(userEmb ! row) <.> (toColumns itemEmb !! col)]


{------------------------------------------------------------------------------}
{-                                   HELPERS                                  -}
{------------------------------------------------------------------------------}

snapExtremeValues :: Matrix -> Matrix
snapExtremeValues = cmap snap
  where
    snap :: Double -> Double
    snap value 
      | value > 5 = 5
      | value < 1 = 1
      | otherwise = value

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

toVector :: Matrix -> Vector
toVector = head . LA.toRows

saveToDb :: Int -> Int -> Double -> EmbeddingPair -> IO ()  
saveToDb kValue iterations mse (userEmb, itemEmb) = do
    mongoInfo <- Db.fetchMongoInfo
    let dto = EmbeddingsDTO kValue mse iterations (LA.toLists userEmb) (LA.toLists itemEmb)
    Db.createEmbeddings mongoInfo dto


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

getRandomFrom :: [a] -> IO a
getRandomFrom [] = error "List cannot be empty"
getRandomFrom lst = do
  g <- Rand.newStdGen
  let (index, _) = Rand.randomR (0, len) g
  return $ lst !! index
  where
    len = length lst - 1


defaultPredictionOptions :: Options
defaultPredictionOptions =
  Options { iterationRange = (2000, 2001)
          , threshold = 0.000000001
          , initialLearningRate = 0.000001
          }


defaultTrainingOptions :: Options
defaultTrainingOptions =
  Options { iterationRange = (1001, 1002)
          , threshold = 0.001
          , initialLearningRate = 0.000001
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

getTrainingMatrix :: Matrix -> IO Matrix
getTrainingMatrix m = (m *) <$> mkRandomMatrix (size m)

rndOneOrZero :: Matrix -> Matrix
rndOneOrZero = cmap (\x -> if x > 0.80 then 0 else 1)

mkMatrix :: (Int, Int) -> [Double] -> Matrix
mkMatrix (rows, cols) = rows><cols

mkRandomMatrix :: (Int, Int) -> IO Matrix
mkRandomMatrix dimensions = rndOneOrZero . mkMatrix dimensions <$> getRandomNumbers


fromDense :: Matrix -> AssocMatrix
fromDense m = filter (\(_, x) -> x /= 0) $ go 0 0
  where
    (rows, cols) = size m

    go :: Int -> Int -> [((Int, Int), Double)]
    go r c | c == cols - 1 = ((r, c), value) : go (r + 1) 0
           | r < rows      = ((r, c), value) : go r (c + 1)
           | otherwise     = []
      where value = m ! r ! c
