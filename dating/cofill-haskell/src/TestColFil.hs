module TestColFil where

import Numeric.LinearAlgebra.HMatrix
import Question 
import System.Random
import ReadData
import CollaborativeFiltering


runEverything =
    do
        allQuestions <- getQuestions
        let questions    =  keepOnlyXFirstAnswers 1000 $ take 600 allQuestions
        let rows         = length $ survey_answers $ head questions
        let cols         = length questions
        let answerLst    = convertQuestionsToList questions
        let answerMatrix = mkMatrix rows cols answerLst
        rndMatrix        <- mkRandomMatrix rows cols
        let testMatrix   = answerMatrix * rndMatrix 
        guess            <- runTest testMatrix
        let elements     = sumElements rndMatrix
        
        print $ meanSquareError $ guess-answerMatrix

keepOnlyXFirstAnswers :: Int -> [Question] -> [Question]
keepOnlyXFirstAnswers amount = fmap (\x -> Question (text x) (take amount (survey_answers x)))

runTest :: Mtx -> IO Mtx
runTest testMatrix =
    do
        let minMaxIter = (10, 50)
        let threshold  = 0.01
        let learnRate  = 0.0000001
        let kValue     = 7

        (u, q) <- gradientDescent minMaxIter threshold learnRate testMatrix 100
        return $ mul u q
    

getTestMatrix :: Mtx -> Mtx -> Mtx
getTestMatrix answerMatrix rndMatrix = answerMatrix * oneZeroMatrix
    where
        oneZeroMatrix = rndOneOrZero rndMatrix
        
rndOneOrZero :: Mtx -> Mtx
rndOneOrZero = cmap (\x -> if x > 0.7 then 0 else 1)

mkMatrix :: Int -> Int -> [Double] -> Mtx
mkMatrix rows cols lst = tr' $ (cols><rows) lst :: Mtx

mkRandomMatrix :: Int -> Int -> IO Mtx
mkRandomMatrix cols rows = rndOneOrZero . mkMatrix cols rows <$> getRandomNumbers

convertQuestionsToList :: [Question] -> [Double]
convertQuestionsToList = 
    concatMap (toScores . survey_answers)

toScores :: [SurveyAnswer] -> [Double]
toScores = map score