{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module API where

import Servant
import Servant.Docs

import Types

-- | The API for this web service.
-- Note that these entries *must* line up with the entries in the
-- `server` definition.
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "userId" Int :> Get '[JSON] User

userAPI :: Proxy UserAPI
userAPI = Proxy

instance ToCapture (Capture "userId" Int) where
  toCapture _ =
    DocCapture "userId"                             -- name
               "(integer) ID of the requested user" -- description

-- | Sample user for documentation
isaac :: User
isaac = User 1 "Isaac Newton" 372 "isaac@newton.co.uk" "1683-3-1" -- (fromGregorian 1683 3 1)

-- | Another sample user.
albert :: User
albert = User 2 "Albert Einstein" 136 "ae@mc2.org" "1905-12-1" -- (fromGregorian 1905 12 1)

instance ToSample User User where
  toSample _ = Just isaac

instance ToSample [User] [User] where
  toSample _ = Just [ isaac, albert ]

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


-- | The unified API for the whole web service:
type FullAPI = UserAPI :<|> DocsAPI :<|> StaticAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy
