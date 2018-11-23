{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
module SchemaEnums where

import           GHC.Generics
import qualified Database.Persist.TH as PTH
import           Data.Aeson (ToJSON, FromJSON)

data Gender =
    Male
  | Female
  | Other
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
PTH.derivePersistField "Gender"
