{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module SchemaEnums where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Database.Persist.TH as PTH
import           GHC.Generics

data Gender =
    Male
  | Female
  | Other
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
PTH.derivePersistField "Gender"
