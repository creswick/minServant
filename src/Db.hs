{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Db where

import qualified Data.ByteString as BS
import           Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ ( sql )
-- import           Database.HsSqlPpp.Ast
import           Database.HsSqlPpp.Syntax
import           Database.HsSqlPpp.Quote
import           Database.HsSqlPpp.Annotation
import           Database.HsSqlPpp.Pretty
import qualified Control.Exception as X

import           Types

-- test :: Statement
-- test = [$sqlStmt|
--   create table $(tablename) (
--    $(varname) $(typename)
--   );
--         |]
--   where
--     tablename = "my_table"
--     varname = "my_field"
--     typename = "text"

query :: (PG.FromRow r) => PG.Connection -> Statement -> IO (Either X.SomeException [r])
query conn stmt = X.try (PG.query_ conn $ fromString $ TL.unpack $ prettyStatements defaultPrettyFlags [stmt])

connInfo :: PG.ConnectInfo
connInfo = PG.defaultConnectInfo
  { PG.connectUser     = "creswick"
  , PG.connectPassword = ""
  , PG.connectDatabase = "servant"
  }

loadUsers :: IO [User]
loadUsers = do
  res <- X.bracket (PG.connect connInfo) PG.close $ \conn -> do
              query conn [$sqlStmt| SELECT * FROM users; |]
  case res of
    Left    err -> return []
    Right users -> return users

loadUser :: Int -> IO (Either String User)
loadUser the_id' = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  let the_id = NumberLit emptyAnnotation $ show the_id'
  eUs <- query conn [$sqlStmt| SELECT *
                                FROM users
                               WHERE user_id=1; -- $(the_id);
                   |]
  case eUs of
    Left err -> return $ Left (show err)
    Right [] -> return $ Left "user not found"
    Right (u:_) -> return $ Right u

