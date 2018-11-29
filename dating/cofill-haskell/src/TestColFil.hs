module TestColFil where

    import qualified Numeric.LinearAlgebra.HMatrix as HM
    import Numeric.LinearAlgebra (size, tr', (><), cmap, sumElements)
    import Question 
    import System.Random
    import DataLoad
    import CollaborativeFiltering
    
    
    runEverything = do
        let answerMatrix = _ :: Matrix
        let (rows, cols) = size answerMatrix
        let mSize        = fromIntegral $ rows * cols
        let testMatrix   = getTestMatrix answerMatrix
        guess            <- runTest testMatrix
        
        print $ meanSquareError (guess-answerMatrix) mSize
                
    runTest :: Matrix -> IO Matrix
    runTest testMatrix =
        do
            let minMaxIter = (10, 50)
            let threshold  = 0.01
            let learnRate  = 0.0000001
            let kValue     = 7
    
            (u, q) <- train minMaxIter threshold learnRate testMatrix 100
            return $ mul u q
        
    
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