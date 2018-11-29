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

import           Data.Text                  (Text)
import           Data.Time.Calendar         (Day)
import           Data.Time.Clock            (UTCTime)
import           Database.Persist.MongoDB
import qualified Database.Persist.TH        as PTH
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax

import           SchemaEnums


let mongoSettings = (PTH.mkPersistSettings (ConT ''MongoContext)) {PTH.mpsGeneric = False}
 in PTH.share [PTH.mkPersist mongoSettings] [PTH.persistLowerCase|
  User sql=users
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
    deriving Show Eq Generic

  Conversation sql=conversations
    members  [Text]
    messages [Message]
    deriving Show Eq Generic

  Message sql=messages
    author    Text
    timestamp UTCTime
    body      Text
    deriving Show Eq Generic

  Question sql=questions
    body    Text
    answers [Answer]
    deriving Show Eq Generic

  Answer sql=answers
    answerer    Text
    score       Int
    timestamp   UTCTime
    isPredicted Bool
    deriving Show Eq Generic

  QuestionEmbedding sql=question_embeddings
    timestamp UTCTime
    embedding [Column]
    deriving Eq Show Generic
|]

type Column = [Double]
