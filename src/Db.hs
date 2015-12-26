{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
module Db where

import qualified Data.ByteString as BS
import           Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ ( sql )
import           Database.HsSqlPpp.Syntax
import           Database.HsSqlPpp.Quote
import           Database.HsSqlPpp.Annotation
import           Database.HsSqlPpp.Pretty
import qualified Control.Exception as X
import           Control.Monad (void)

import           Types

query :: (PG.FromRow r) => PG.Connection -> Statement -> IO (Either X.SomeException [r])
query conn stmt = X.try (PG.query_ conn $ fromString $ TL.unpack $ prettyStatements defaultPrettyFlags [stmt])

exec :: PG.Connection -> Statement -> IO (Either X.SomeException ())
exec conn stmt = X.try (void $ PG.execute_ conn $ fromString $ TL.unpack $ prettyStatements defaultPrettyFlags [stmt])

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
                               WHERE $e(the_id);
                   |]
  case eUs of
    Left err -> return $ Left (show err)
    Right [] -> return $ Left "user not found"
    Right (u:_) -> return $ Right u

saveUser :: User -> IO (Either String [()])
saveUser user = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  res <- exec conn [$sqlStmt| INSERT INTO users
                                      (name, age, email, registration_date)
                               VALUES ($e(newname), $e(newage), $e(newemail), $e(registrationdate));
                             |]
  case res of
    Left err -> return (Left $ show err)
    Right  v -> return $ Right [()]

  where
    newname = StringLit emptyAnnotation (name user)
    newage = NumberLit emptyAnnotation (show $ age user)
    newemail = StringLit emptyAnnotation (email user)
    registrationdate = StringLit emptyAnnotation (registration_date user)

-- insert into users (name, age, email, registration_date) values ('A. E.', 136, 'ae@mc2.org', '1905-12-1');
