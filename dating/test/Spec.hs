module Main where

import           Data.Either                (isRight)
import           Test.Hspec
import           Test.QuickCheck

import qualified Database                   as DB
import           FrontendTypes
import           Recommendation.Recommender (fromDense, toDense, Matrix)

main :: IO ()
main = quickCheck prop_fromDenseAndBack


prop_fromDenseAndBack :: Matrix -> Bool
prop_fromDenseAndBack m = m == (toDense . fromDense) m

testUserCreationAndLogin = describe "users" $
  it "creates a user" $ do
    mongoInfo <- DB.fetchMongoInfo
    createUserDTO <- generate arbitrary :: IO CreateUserDTO
    eitherLoggedInDTO <- DB.createUser mongoInfo createUserDTO
    isRight eitherLoggedInDTO `shouldBe` True