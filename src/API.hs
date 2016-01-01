{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module API where


import Data.Text (Text)
import Servant
import Servant.Docs

import Auth
import Types

-- | The API for this web service.
-- Note that these entries *must* line up with the entries in the
-- `server` definition.
type NoteAPI = "notes" :> Get '[JSON] [Note]
          :<|> "note" :> Capture "noteId" Int :> Get '[JSON] Note
          :<|> "addnote" :> ReqBody '[JSON] Note :> Post '[JSON] [Note]

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

instance ToCapture (Capture "noteId" Int) where
  toCapture _ =
    DocCapture "noteId"                             -- name
               "(integer) ID of the requested note" -- description

-- | Sample note for documentation
note1 :: Note
note1 = Note 1 "Test Note 1" "This is a sample note." "1683-3-1"

-- | Another sample note.
note2 :: Note
note2 = Note 2 "Another sample note" "Just a sample note, with multiple lines.\nHere's the second line." "1905-12-1"

instance ToSample Note where
  toSamples _ = [("Sample Note", note1)]

instance ToSample [Note] where
  toSamples _ = [("Sample notes", [ note1, note2 ])]

-- | API for the documentation end point, which returns results as markdown:
type DocsAPI = "docs" :> Raw

docsAPI :: Proxy DocsAPI
docsAPI = Proxy

-- | API for static file hosting, either in the static sub directory,
-- or as root (for index.html)
type StaticAPI = "static" :> Raw -- Static file handler.
            :<|> Raw -- Handler for requests to the root url ("/")

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

-- type TestAPI = NoteAPI :<|> AuthAPI

type ClientAPI = NoteAPI :<|> AuthAPI

clientAPI :: Proxy ClientAPI
clientAPI = Proxy

-- | The unified API for the whole web service:
type FullAPI = NoteAPI :<|> DocsAPI :<|> StaticAPI :<|> AuthAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy
