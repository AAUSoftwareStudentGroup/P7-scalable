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
    beingPredicted  Bool
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
    index   Int
    text    Text
    answers [Answer]
    deriving Show Eq Generic

  Answer sql=answers
    answerer    Text
    score       Double
    timestamp   UTCTime
    ispredicted Bool
    deriving Show Eq Generic

  Embeddings sql=embeddings
    timestamp  UTCTime
    kValue     Int
    mse        Double
    iterations Int
    userEmb    [Column]
    itemEmb    [Column]
    deriving Eq Show Generic

  UserMatches sql=user_matches
    match       [Text]
    correlation Double
    deriving Eq Show Generic

  OldQuestion sql=questions
    index   Int
    text    Text
    answers [OldAnswer]
    deriving Show Eq Generic

  OldAnswer sql=answers
    answerer    Text
    score       Int
    timestamp   UTCTime
    ispredicted Bool
    deriving Show Eq Generic
|]

type Column = [Double]
