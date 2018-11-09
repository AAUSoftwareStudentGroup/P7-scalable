module Main where

import           Data.Maybe      (isJust)
import qualified Database        as DB
import           FrontendTypes
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "users" $ do
    it "creates a user" $ do
      mongoInfo <- DB.fetchMongoInfo
      createUserDTO <- generate arbitrary :: IO CreateUserDTO
      maybeLoggedInDTO <- DB.createUser mongoInfo createUserDTO
      isJust maybeLoggedInDTO `shouldBe` True




{-
insert user1
insert user2
insert message, which creates convo
fetch convo
-}
