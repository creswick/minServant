{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module API where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Servant
import Servant.Docs


import Types

-- | The API for this web service.
-- Note that these entries *must* line up with the entries in the
-- `server` definition.
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "userId" Int :> Get '[JSON] User
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User

userAPI :: Proxy UserAPI
userAPI = Proxy

instance ToCapture (Capture "userId" Int) where
  toCapture _ =
    DocCapture "userId"                             -- name
               "(integer) ID of the requested user" -- description

instance ToSample User User where
  toSample _ = Just (User 1 "Isaac Newton" 372 "isaac@newton.co.uk" "1683-3-1")

instance ToSample [User] [User] where
  toSample _ = Just [ User 1 "Isaac Newton" 372 "isaac@newton.co.uk" "1683-3-1"
                    , User 2 "Albert Einstein" 136 "ae@mc2.org" "1905-12-1" ]

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] userAPI

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

type FullAPI = UserAPI :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy
