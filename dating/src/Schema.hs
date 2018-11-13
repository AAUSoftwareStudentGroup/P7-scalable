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

import           Data.Aeson
import           Data.ByteString               (ByteString)
import           Data.Int                      (Int64)
import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (UTCTime)
import           Database.Persist              (Entity (..), Key)
import           Database.Persist.MongoDB
import qualified Database.Persist.TH           as PTH
import           Elm                           (ElmType)
import           Elm.Export.Persist
import           Elm.Export.Persist.BackendKey ()
import           GHC.Generics                  (Generic, Rep)
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
|]
