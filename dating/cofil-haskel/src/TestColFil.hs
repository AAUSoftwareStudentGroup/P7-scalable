module TestColFil where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix

import Question 
import System.Random

import ReadData

import CollaborativeFiltering


---main :: IO ()
main =
    do
        allQuestions <- getQuestions
        let questions =  take 300 allQuestions
        let rows = length $ survey_answers $ head questions
        let cols = length questions
        let answerLst = convertQuestionsToList questions
        let answerMatrix = mkMatrix rows cols answerLst
        rndMatrix <- mkRandomMatrix rows cols
        let testMatrix = answerMatrix * rndMatrix 
        guess <- runTest testMatrix
        let elements = sumElements rndMatrix
        --print $ sumElements  rndMatrix
        print elements
        --print(meanSquareError(guess-answerMatrix))
        --print $ getScores $ survey_answers $ head questions
        --print (tr' $ takeColumns 1 answerMatrix )

getScores lst = map (\x -> score x) lst

keepOnlyXFirstAnswers :: Int -> [Question] -> [Question]
keepOnlyXFirstAnswers amount questions = fmap (\x -> Question (text x) (take amount (survey_answers x))) questions

--runTest :: Matrix R -> Int
runTest testMatrix =
    do
        (u, q) <- gradientDescent threshold' alpha' testMatrix 5
        return $ mul u q
    

getTestMatrix :: Matrix R -> Matrix R -> Matrix R
getTestMatrix answerMatrix rndMatrix = answerMatrix * oneZeroMatrix
    where
        oneZeroMatrix = rndOneOrZero rndMatrix
        
rndOneOrZero :: Matrix R -> Matrix R
rndOneOrZero m = cmap (\x -> if x > 0.7 then 0 else 1) m

mkMatrix :: Int -> Int -> [Double] -> Matrix R
mkMatrix rows cols lst = tr' $ (cols><rows) lst :: Matrix R

mkRandomMatrix :: Int -> Int -> IO (Matrix R)
mkRandomMatrix cols rows = rndOneOrZero <$> mkMatrix cols rows <$> getRandomNumbers

convertQuestionsToList :: [Question] -> [R]
convertQuestionsToList lst = 
    concat $ fmap (\x -> convertAnswersToList $ survey_answers x) lst

convertAnswersToList :: [SurveyAnswer] -> [R]
convertAnswersToList lst = fmap (\x -> score x) lst