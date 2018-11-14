module CollaborativeFiltering where

    import Numeric.LinearAlgebra
    import Numeric.LinearAlgebra.HMatrix
    
    
    group_answers = (6><10)
        [ 3, 4, 3, 1, 1, 5, 5, 2, 3, 3
        , 3, 2, 3, 1, 4, 5, 5, 3, 2, 1
        , 1, 1, 3, 1, 3, 1, 4, 3, 2, 2
        , 4, 1, 3, 1, 2, 4, 5, 3, 1, 2
        , 2, 4, 2, 1, 2, 5, 5, 2, 1, 1
        , 3, 4, 2, 1, 2, 5, 5, 4, 1, 1 ] :: Matrix R
    
    test_answers = (6><10)
        [ 0, 4, 3, 1, 1, 5, 5, 0, 3, 3
        , 3, 2, 3, 0, 4, 5, 5, 3, 2, 1
        , 1, 1, 3, 1, 3, 1, 4, 3, 0, 2
        , 4, 1, 3, 1, 2, 0, 5, 0, 1, 2
        , 2, 4, 0, 1, 2, 5, 0, 2, 1, 1
        , 3, 4, 2, 1, 2, 5, 5, 4, 0, 1 ] :: Matrix R
    
    toOneOrZero :: Matrix R -> Matrix R
    toOneOrZero m = cmap (\x -> if x == 0 then 0 else 1) m
    
    type EmbeddingMatrix = Matrix R
    type AnswerMatrix = Matrix R
    type LearningRate = Matrix R
    
    mkEmbeddinMatrix :: Int -> Int -> EmbeddingMatrix
    mkEmbeddinMatrix rows cols = (rows><cols) [1, 1 ..] :: EmbeddingMatrix
    
    alpha' :: LearningRate
    alpha' = 0.0001
    
    threshold' = 0.0000000000001
    
    runner = mul u q
      where
        (u, q) = gradientDescent threshold' alpha' test_answers 4
    
    mse = meanSquareError(runner-group_answers)
    
    gradientDescent :: Double -> LearningRate -> AnswerMatrix -> Int -> (EmbeddingMatrix, EmbeddingMatrix)
    gradientDescent threshold learningRate answers kValue = 
        gradientDescent' threshold learningRate Nothing answers (toOneOrZero answers) userEmbedding questEmbedding
        where
            (rows, cols)   = size answers
            userEmbedding  = mkEmbeddinMatrix rows kValue
            questEmbedding = mkEmbeddinMatrix kValue cols
    
    gradientDescent' :: Double -> LearningRate -> Maybe Double -> AnswerMatrix -> AnswerMatrix -> EmbeddingMatrix -> EmbeddingMatrix -> (EmbeddingMatrix, EmbeddingMatrix)
    gradientDescent' threshold alpha preMse a a' u q = 
        if outsideThreshold threshold mse preMse
        then gradientDescent' threshold alpha (Just mse) a a' u' q'
        else (u, q)
      where
        guess  = mul u q
        guess' = guess * a'
        error  = guess' - a
        mse   = meanSquareError (a - guess')
        
        u'     = u - (tr' $ mul q (tr' error)) * alpha
        q'     = q - (mul (tr' u) error) * alpha
    
    -- Maybe Todo: Can we find a better stop condition?
    outsideThreshold :: Double -> Double -> Maybe Double -> Bool
    outsideThreshold _ _ Nothing = True
    outsideThreshold _ newMse (Just oldMse) = newMse /= oldMse
    --outsideThreshold threshold newMse (Just oldMse) =
    --    threshold + newMse < oldMse && newMse < oldMse
    
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