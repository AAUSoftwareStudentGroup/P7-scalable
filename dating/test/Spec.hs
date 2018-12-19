module Main where

  import           Data.Either                (isRight)
  import           Test.Hspec
  import           Test.QuickCheck
  
  import qualified Database                   as DB
  import           FrontendTypes
  import           Recommendation.Recommender (fromDense, toDense, Matrix, sortBySndDesc)
  
  main :: IO ()
  main = hspec $ do
    describe "sortBySndDesc" $
      it "sorts a list of tuples by the second element in descending order" $
        sortBySndDesc [('c', 1), ('a', 3), ('z', 2)] 
        `shouldBe` 
        [('a', 3), ('z', 2), ('c', 1)]
    describe "fromDense" $
      it "fromDense composed with toDense is identity" $
        property (\m -> m == (toDense . fromDense) m)
    describe "users" $
      it "creates a user" $ do
        mongoInfo <- DB.fetchMongoInfo
        createUserDTO <- generate arbitrary :: IO CreateUserDTO
        eitherLoggedInDTO <- DB.createUser mongoInfo createUserDTO
        isRight eitherLoggedInDTO `shouldBe` True