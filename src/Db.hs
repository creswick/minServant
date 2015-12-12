{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Db where

import           Data.ByteString
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ ( sql )
import qualified Control.Exception as X

import           Types

connInfo :: ConnectInfo
connInfo = defaultConnectInfo
  { connectUser     = "creswick"
  , connectPassword = ""
  , connectDatabase = "servant"
  }

loadUsers :: IO [User]
loadUsers = X.bracket (connect connInfo) PG.close $ \conn -> do
  PG.query_ conn [sql| SELECT * FROM users |]

loadUser :: Int -> IO (Either String User)
loadUser theId = X.bracket (connect connInfo) PG.close $ \conn -> do
  us <- PG.query conn [sql| SELECT *
                           FROM users
                           WHERE user_id=?
                          |] (PG.Only theId)
  case us of
    [] -> return $ Left "user not found"
    (u:_) -> return $ Right u
