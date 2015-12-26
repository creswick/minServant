{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Data.Aeson
import GHC.Generics
import Data.Time.Calendar (Day)
import qualified Database.PostgreSQL.Simple.FromField as D
import qualified Database.PostgreSQL.Simple.FromRow   as D

data User = User
  { user_id :: Int
  , name :: String
  , age :: Int
  , email :: String
  , registration_date :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

instance D.FromRow User where
  fromRow = do
    user_id <- D.field
    name <- D.field
    age <- D.field
    email <- D.field
    registration_date <- D.field
    return User {..}

-- instance D.FromField Day where
--   fromField f mdata = do
--     dayStr <- D.fromField f mdata
--     return $ read dayStr
