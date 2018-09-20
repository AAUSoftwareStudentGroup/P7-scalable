{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)
import           Data.Aeson.Types (Parser, Pair)
import           Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)
import           Elm (ElmType)
import           GHC.Generics

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    email       Text
    password    Text
    username    Text
    gender      Gender
    birthday    Day
    town        Text
    profileText Text
    UniqueEmail email
    UniqueUser  username
    deriving Show Read Eq Generic

  Message sql=messages
    sender    UserId
    receiver  UserId
    timeStamp UTCTime
    content   Text
    deriving Show Read Eq Generic
|]


data Gender =
    Male
  | Female
  | Other
  deriving (Show, Read, Eq)
PTH.derivePersistField "Gender"


-- USER

instance ElmType User

instance ToJSON User where
  toJSON user = object
    [ "userEmail"       .= userEmail user
    , "userPassword"    .= userPassword user
    , "userUsername"    .= userUsername user
    , "userGender"      .= userGender user
    , "userBirthday"    .= userBirthday user
    , "userTown"        .= userTown user
    , "userProfileText" .= userProfileText user
    ]

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uEmail       <- o .: "userEmail"
  uPassword    <- o .: "userPassword"
  uUsername    <- o .: "userUsername"
  uGender      <- o .: "userGender"
  uBirthday    <- o .: "userBirthday"
  uTown        <- o .: "userTown"
  uProfileText <- o .: "userProfileText"
  return User
    { userEmail       = uEmail
    , userPassword    = uPassword
    , userUsername    = uUsername
    , userGender      = uGender
    , userBirthday    = uBirthday
    , userTown        = uTown
    , userProfileText = uProfileText
    }


-- MESSAGE

instance ElmType Message

instance ToJSON Message where
  toJSON msg = object
    [ "messageSender"    .= messageSender msg
    , "messageReceiver"  .= messageReceiver msg
    , "messageTimeStamp" .= messageTimeStamp msg
    , "messageContent"   .= messageContent msg
    ]

instance FromJSON Message where
  parseJSON = withObject "Message" parseMessage


parseMessage :: Object -> Parser User
parseMessage o = do
  mSender    <- o .: "messageSender"
  mReceiver  <- o .: "messageReceiver"
  mTimeStamp <- o .: "messageTimeStamp"
  mContent   <- o .: "messageContent"
  return Message
    { messageSender    = mSender
    , messageReceiver  = mReceiver
    , messageTimeStamp = mTimeStamp
    , messageContent   = mContent
    }
