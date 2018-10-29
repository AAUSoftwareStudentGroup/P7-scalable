{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module FrontendTypes where

import           Data.Aeson.Types   (FromJSON, ToJSON)
import           Data.Int           (Int64)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime (..))
import           GHC.Generics       (Generic)
import           SchemaEnums        (Gender)


-- Users

data CreateUserData = CreateUserData
  { email       :: Text
  , password    :: Text
  , username    :: Text
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data UserData = UserData
  { username    :: Text
  , userId      :: Int64
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


-- Authentication

data CredentialData = CredentialData
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data LoggedInData = LoggedInData
  { username  :: Text
  , userId    :: Int64
  , authToken :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Messages

data ConversationPreviewData = ConversationPreviewData
  { convoWithUsername :: Text
  , convoWithId       :: Int64
  , isLastAuthor      :: Bool
  , body              :: Text
  , timeStamp         :: UTCTime
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


newtype CreateMessageData = CreateMessageData
  { body :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data MessageData = MessageData
  { authorName :: Text
  , timeStamp  :: UTCTime
  , body       :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

