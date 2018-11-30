{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FrontendTypes where

import           Data.Aeson
import           Data.Text.Arbitrary
import           Data.Time.Calendar             (Day)
import           Data.Time.Clock                (UTCTime (..))
import           GHC.Generics                   (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ()

import           SchemaEnums                    (Gender (..))

{-----------------------------------------------------------------------------}
{-                                  USERS                                    -}
{-----------------------------------------------------------------------------}

data CreateUserDTO = CreateUserDTO
  { email       :: Text
  , password    :: Text
  , username    :: Text
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  , imageData   :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data EditUserDTO = EditUserDTO
  { password    :: Maybe Text
  , gender      :: Maybe Gender
  , birthday    :: Maybe Day
  , town        :: Maybe Text
  , profileText :: Maybe Text
  , imageData   :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UserDTO = UserDTO
  { username    :: Text
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  , imageUrl    :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


{-----------------------------------------------------------------------------}
{-                             AUTHENTICATION                                -}
{-----------------------------------------------------------------------------}

data CredentialDTO = CredentialDTO
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data LoggedInDTO = LoggedInDTO
  { username  :: Text
  , authToken :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


{-----------------------------------------------------------------------------}
{-                              CONVERSATIONS                                -}
{-----------------------------------------------------------------------------}

data ConversationPreviewDTO = ConversationPreviewDTO
  { convoWithUsername :: Text
  , isLastAuthor      :: Bool
  , body              :: Text
  , timeStamp         :: UTCTime
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


newtype CreateMessageDTO = CreateMessageDTO
  { body :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data ConversationDTO = ConversationDTO
  { convoWithUsername :: Text
  , messages          :: [MessageDTO]
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data MessageDTO = MessageDTO
  { authorUsername :: Text
  , timeStamp      :: UTCTime
  , body           :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)



{-----------------------------------------------------------------------------}
{-                                 Questions                                 -}
{-----------------------------------------------------------------------------}

data QuestionDTO = QuestionDTO
  { id       :: Text
  , question :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data AnswerDTO = AnswerDTO
  { id    :: Text
  , score :: Int
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data QuestionEmbeddingDTO = QuestionEmbeddingDTO
  { mse        :: Double
  , iterations :: Int
  , embedding  :: [[Double]]
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

{------------------------------------------------------------------------------}
{-                           ARBITRARY INSTANCES                              -}
{------------------------------------------------------------------------------}

instance Arbitrary Gender where
  arbitrary = frequency [ (2, pure Male)
                        , (2, pure Female)
                        , (1, pure Other)
                        ]

instance Arbitrary CreateUserDTO where
  arbitrary = CreateUserDTO
              <$> arbitrary -- email
              <*> arbitrary -- password
              <*> arbitrary -- username
              <*> arbitrary -- gender
              <*> arbitrary -- birthday
              <*> arbitrary -- town
              <*> arbitrary -- profileText
              <*> arbitrary -- image


instance Arbitrary UserDTO where
  arbitrary = UserDTO
              <$> arbitrary -- username
              <*> arbitrary -- gender
              <*> arbitrary -- birthday
              <*> arbitrary -- town
              <*> arbitrary -- profileText
              <*> arbitrary -- base64

instance Arbitrary CredentialDTO where
  arbitrary = CredentialDTO
              <$> arbitrary -- username
              <*> arbitrary -- password

instance Arbitrary LoggedInDTO where
  arbitrary = LoggedInDTO
              <$> arbitrary -- username
              <*> arbitrary -- authToken

instance Arbitrary ConversationPreviewDTO where
  arbitrary = ConversationPreviewDTO
              <$> arbitrary -- convoWithUsername
              <*> arbitrary -- isLastAuthor
              <*> arbitrary -- body
              <*> arbitrary -- timeStamp

instance Arbitrary CreateMessageDTO where
  arbitrary = CreateMessageDTO
              <$> arbitrary -- body

instance Arbitrary ConversationDTO where
  arbitrary = ConversationDTO
              <$> arbitrary -- convoWithUsername
              <*> arbitrary -- messages

instance Arbitrary MessageDTO where
  arbitrary = MessageDTO
              <$> arbitrary -- autherUsername
              <*> arbitrary -- timeStamp
              <*> arbitrary -- body

instance Arbitrary QuestionEmbeddingDTO where
  arbitrary = QuestionEmbeddingDTO
              <$> arbitrary -- mse
              <*> arbitrary -- iterations
              <*> arbitrary -- embedding
