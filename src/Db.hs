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

loadNotes :: IO [Note]
loadNotes = do
  res <- X.bracket (PG.connect connInfo) PG.close $ \conn -> do
              query conn [sqlStmt| SELECT note_id, title, content, note_date
                                   FROM notes;
                                 |]
  case res of
    Left    err -> do putStrLn ("Error loading notes: "++show err)
                      return []
    Right notes -> return notes

loadNote :: Int -> IO (Either String Note)
loadNote the_id' = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  let the_id = NumberLit emptyAnnotation $ show the_id'
  eUs <- query conn [sqlStmt| SELECT *
                              FROM notes
                              WHERE $e(the_id);
                    |]
  case eUs of
    Left err -> do putStrLn ("Error loading note: "++show err)
                   return $ Left (show err)
    Right [] -> return $ Left "note not found"
    Right (u:_) -> return $ Right u

saveNote :: Note -> IO (Either String [()])
saveNote note = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  res <- exec conn [sqlStmt| INSERT INTO notes
                                    (title, content, user_id)
                             VALUES ($e(newtitle), $e(newcontent), $e(userid));
                           |]
  case res of
    Left err -> do putStrLn ("Error saving note: "++show err)
                   return (Left $ show err)
    Right  v -> return $ Right [()]

  where
    newtitle = StringLit emptyAnnotation (title note)
    newcontent = StringLit emptyAnnotation (content note)
    userid = NumberLit emptyAnnotation (show 2) -- TODO get current user via session cookie.

-- insert into users (name, age, email, registration_date) values ('A. E.', 136, 'ae@mc2.org', '1905-12-1');
newSession :: Int -> IO (Either String String)
newSession userId = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  res <- query conn [sqlStmt| INSERT INTO sessions (user_id)
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

-- | Invalidate a client session.
clearSessionCookie :: BS.ByteString -> IO (Either X.SomeException ())
clearSessionCookie sessCookie = X.bracket (PG.connect connInfo) PG.close $ \conn -> do
  exec conn [sqlStmt| DELETE FROM sessions
                      WHERE session_id=$e(sesscookie);
                    |]
  where
    parseSessionCookie = drop 5 $ C8.unpack sessCookie
    sesscookie = StringLit emptyAnnotation parseSessionCookie
