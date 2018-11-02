{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

module FrontendTypes where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Int           (Int64)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime (..))
import           GHC.Generics       (Generic)

import           SchemaEnums        (Gender)

-- Users

data CreateUserDTO = CreateUserDTO
  { email       :: Text
  , password    :: Text
  , username    :: Text
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data UserDTO = UserDTO
  { username    :: Text
  , userId      :: Text
  , gender      :: Gender
  , birthday    :: Day
  , town        :: Text
  , profileText :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


-- Authentication

data CredentialDTO = CredentialDTO
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data LoggedInDTO = LoggedInDTO
  { username  :: Text
  , userId    :: Text
  , authToken :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Messages

data ConversationPreviewDTO = ConversationPreviewDTO
  { convoWithUsername :: Text
  , convoWithId       :: Text
  , isLastAuthor      :: Bool
  , body              :: Text
  , timeStamp         :: UTCTime
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


newtype CreateMessageDTO = CreateMessageDTO
  { body :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data MessageDTO = MessageDTO
  { authorName :: Text
  , timeStamp  :: UTCTime
  , body       :: Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
