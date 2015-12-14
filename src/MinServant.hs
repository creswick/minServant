{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module MinServant where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Time.Calendar (fromGregorian)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left, EitherT)
import qualified Control.Exception as X
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

fileNotFound :: FilePath -> ServantErr
fileNotFound filepath = err404 { errBody = LC8.pack ("File not found: " ++ filepath) }

docsServer _ respond =
  respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

staticServer :: Server StaticAPI
staticServer = serveDirectory "static"
          :<|> serveDirectory "static"

server :: Server FullAPI
server = userServer :<|> docsServer :<|> staticServer

app :: Application
app = serve fullAPI server
