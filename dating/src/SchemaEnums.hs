{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
module SchemaEnums where

import           Elm (ElmType)
import           GHC.Generics
import qualified Database.Persist.TH as PTH
import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)

data Gender =
    Male
  | Female
  | Other
  deriving (Show, Read, Eq, Generic, ElmType, ToJSON, FromJSON)
PTH.derivePersistField "Gender"
