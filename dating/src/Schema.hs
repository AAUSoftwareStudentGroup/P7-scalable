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

import           Data.Int                      (Int64)
import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (UTCTime)
import           Database.Persist              (Entity (..), Key)
import qualified Database.Persist.TH           as PTH
import           Elm                           (ElmType)
import           Elm.Export.Persist
import           Elm.Export.Persist.BackendKey ()
import           GHC.Generics                  (Generic)
import           SchemaEnums

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json sql=users
    email           Text
    password        Text
    username        Text
    gender          Gender
    birthday        Day
    town            Text
    profileText     Text
    authToken       Text
    UniqueEmail     email
    UniqueUser      username
    UniqueAuthToken authToken
    deriving Show Read Eq Generic

  Conversation json sql=conversations
    userOneId UserId
    userTwoId UserId
    deriving Show Read Eq Generic

  Message json sql=messages
    conversationId ConversationId
    authorId       UserId
    timeStamp      UTCTime
    body           Text
    deriving Show Read Eq Generic
|]

instance ElmType User
instance ElmType Message
instance ElmType Conversation

deriving instance ElmType UserId
deriving instance ElmType MessageId
deriving instance ElmType ConversationId
