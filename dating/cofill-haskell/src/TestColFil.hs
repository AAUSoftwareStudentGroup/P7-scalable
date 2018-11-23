module TestColFil where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix

import Question 
import System.Random

import ReadData

import CollaborativeFiltering


runEverything =
    do
        allQuestions <- getQuestions
        let questions =  keepOnlyXFirstAnswers 1000 $ take 600 allQuestions
        let rows = length $ survey_answers $ head questions
        let cols = length questions
        let answerLst = convertQuestionsToList questions
        let answerMatrix = mkMatrix rows cols answerLst
        rndMatrix <- mkRandomMatrix rows cols
        let testMatrix = answerMatrix * rndMatrix 
        guess <- runTest testMatrix
        let elements = sumElements rndMatrix
        --print $ sumElements  rndMatrix
        --print elements
        print(meanSquareError(guess-answerMatrix))
        --print $ getScores $ survey_answers $ head questions
        --print (tr' $ takeColumns 1 answerMatrix )



keepOnlyXFirstAnswers :: Int -> [Question] -> [Question]
keepOnlyXFirstAnswers amount = fmap (\x -> Question (text x) (take amount (survey_answers x)))

runTest :: Matrix R -> IO AnswerMatrix
runTest testMatrix =
    do
        (u, q) <- gradientDescent threshold' alpha' testMatrix 100
        return $ mul u q
    

getTestMatrix :: Matrix R -> Matrix R -> Matrix R
getTestMatrix answerMatrix rndMatrix = answerMatrix * oneZeroMatrix
    where
        oneZeroMatrix = rndOneOrZero rndMatrix
        
rndOneOrZero :: Matrix R -> Matrix R
rndOneOrZero = cmap (\x -> if x > 0.7 then 0 else 1)

mkMatrix :: Int -> Int -> [Double] -> Matrix R
mkMatrix rows cols lst = tr' $ (cols><rows) lst :: Matrix R

mkRandomMatrix :: Int -> Int -> IO (Matrix R)
mkRandomMatrix cols rows = rndOneOrZero . mkMatrix cols rows <$> getRandomNumbers

convertQuestionsToList :: [Question] -> [R]
convertQuestionsToList = 
    concatMap (toScores . survey_answers)

toScores :: [SurveyAnswer] -> [R]
toScores = map score