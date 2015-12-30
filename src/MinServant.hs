{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module MinServant where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except 
import qualified Control.Exception as X
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Time.Calendar (fromGregorian)
import           Network.HTTP.Types
import           Network.Wai
import           Servant
import           Servant.Docs (markdown, docsWithIntros, DocIntro(..))
import           Servant.JS.JQuery

import           API
import           Db
import           Errors
import           Types

-- | The set of logical handlers that go with the API.
--
-- As mentioned above, these handlers must be listed in the same order
-- as the API entries.  If they do not, then you will (hopefully) get
-- a type error, but if you happen to swap two handlers that have the
-- same type, then the logical definitions will not match, and your
-- app will run happily -- while generating the wrong results.
noteServer :: Server NoteAPI
noteServer = notes :<|> getNote :<|> newNote

-- | The notes endpoint loads the full list of notes from a database,
-- which has to happen in IO, so the return type needs to incorporate
-- error conditions.
notes :: ExceptT ServantErr IO [Note]
notes = liftIO loadNotes

getNote :: Int -> ExceptT ServantErr IO Note
getNote theId = do
  res <- liftIO $ loadNote theId
  case res of
    Left  _err -> throwE noteNotFound
    Right note -> return note

newNote :: Note -> ExceptT ServantErr IO [Note]
newNote newNote = do
  res <- liftIO $ saveNote newNote
  case res of
    Left _err -> throwE serverError
    Right  _  -> notes

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] noteAPI

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

docsServer _ respond =
  respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

staticServer :: Server StaticAPI
staticServer = serveDirectory "static"
          :<|> serveDirectory "static"

server :: Server FullAPI
server = noteServer :<|> docsServer :<|> staticServer

app :: Application
app = serve fullAPI server
