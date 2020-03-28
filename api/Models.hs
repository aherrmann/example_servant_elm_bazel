{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)
import Servant.Elm (deriveBoth)

newtype Id = Id Int
  deriving (Eq, FromHttpApiData, Ord, Show, ToHttpApiData)

data Entry a = Entry
  { entryId :: Id,
    entryValue :: a
  }
  deriving (Eq, Generic, Ord, Show)

data Status
  = Open
  | Done
  deriving (Eq, Generic, Ord, Show)

data Task = Task
  { taskLabel :: Text,
    taskStatus :: Status
  }
  deriving (Eq, Generic, Ord, Show)

deriveBoth (aesonPrefix camelCase) ''Id

deriveBoth (aesonPrefix camelCase) ''Entry

deriveBoth (aesonPrefix camelCase) ''Status

deriveBoth (aesonPrefix camelCase) ''Task
