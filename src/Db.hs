{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Db where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
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


-- TODO: Look into this: http://www.parsonsmatt.org/2015/06/07/servant-persistent.html

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
newSession :: Int -> IO (Either String String)
newSession userId = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  res <- query conn [$sqlStmt| INSERT INTO sessions (user_id)
                               VALUES ($e(userid))
                               RETURNING cast(session_id as text);
                             |]
  case res of
    Left      err -> return (Left $ show err)
    Right      [] -> return $ Left "user not found"
    Right (PG.Only res:_) -> return $ Right ("sess="++res)

  where
    userid = NumberLit emptyAnnotation (show userId)

-- | Check a session cookie to see if it is currently valid.
validSessionCookie :: BS.ByteString -> IO Bool
validSessionCookie sessCookie = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  res :: Either X.SomeException [PG.Only String]
      <- query conn [sqlStmt| SELECT cast(session_id as text)
                                FROM sessions
                               WHERE session_id=$e(sesscookie);
                            |]
  case res of
    Left _err -> do putStrLn ("Error: "++show _err)
                    return False
    Right  [] -> do putStrLn "No session cookies found"
                    return False
    Right   _ -> return True
  where
    parseSessionCookie = drop 5 $ C8.unpack sessCookie -- drop the "sess=" prefix.  TODO this should be done with an actual parser.
    sesscookie = StringLit emptyAnnotation parseSessionCookie
