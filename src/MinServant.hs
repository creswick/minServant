{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module MinServant where

import Data.Time.Calendar (fromGregorian)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left, EitherT)
import Network.Wai
import Servant
import Network.HTTP.Types

import Types
import Db
import API

-- | The set of logical handlers that go with the API.
--
-- As mentioned above, these handlers must be listed in the same order
-- as the API entries.  If they do not, then you will (hopefully) get
-- a type error, but if you happen to swap two handlers that have the
-- same type, then the logical definitions will not match, and your
-- app will run happily -- while generating the wrong results.
userServer :: Server UserAPI
userServer = users
    :<|> getUser
    :<|> return albert
    :<|> return isaac

users :: EitherT ServantErr IO [User]
users = liftIO loadUsers

getUser :: Int -> EitherT ServantErr IO User
getUser theId = do
  res <- liftIO $ loadUser theId
  case res of
    Left  _err -> left userNotFound
    Right user -> return user

-- | A simple "user not found" 404 error.
userNotFound :: ServantErr
userNotFound = err404 { errBody = "User does not exist." }


docsServer _ respond =
  respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

staticServer = serveDirectory "static"

server :: Server FullAPI
server = userServer :<|> staticServer :<|> docsServer

app :: Application
app = serve fullAPI server
