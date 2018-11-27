{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (UTCTime)
import           Database.Persist.MongoDB
import qualified Database.Persist.TH           as PTH
import           GHC.Generics                  (Generic)
import           Language.Haskell.TH.Syntax

import           SchemaEnums


let mongoSettings = (PTH.mkPersistSettings (ConT ''MongoContext)) {PTH.mpsGeneric = False}
 in PTH.share [PTH.mkPersist mongoSettings] [PTH.persistLowerCase|
  User json sql=users
    email           Text
    password        Text
    username        Text
    gender          Gender
    birthday        Day
    town            Text
    profileText     Text
    image           Text
    authToken       Text
    salt            Text
    UniqueEmail     email
    UniqueUsername  username
    UniqueAuthToken authToken
    deriving Show Read Eq Generic

  Conversation json sql=conversations
    members  [Text]
    messages [Message]
    deriving Show Read Eq Generic

  Message json sql=messages
    author   Text
    time     UTCTime
    text     Text
    deriving Show Read Eq Generic

  Question json sql=questions
    text            Text
    survey_answers  [SurveyAnswer]
    user_answers    [UserAnswer]
    deriving Show Read Eq Generic

  SurveyAnswer json sql=survey_answers
    respondent_id   Int
    score           Int
    deriving Show Read Eq Generic

  UserAnswer json sql=user_answers
    username        Text
    score           Int
    time            UTCTime
    deriving Show Read Eq Generic

|]
