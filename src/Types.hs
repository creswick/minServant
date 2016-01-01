{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types where

import Data.Aeson
import GHC.Generics
import Data.Time.Calendar (Day)
import           Data.Time                        (UTCTime)
import qualified Database.PostgreSQL.Simple.FromField as D
import qualified Database.PostgreSQL.Simple.FromRow   as D

data Note = Note
  { note_id :: Int
  , title :: String
  , content :: String
  , note_date :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Note
instance FromJSON Note

instance D.FromRow Note where
  fromRow = do
    note_id <- D.field
    title <- D.field
    content <- D.field
    theDate :: UTCTime
            <- D.field
    let note_date = show theDate
    return Note {..}
