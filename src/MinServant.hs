{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module MinServant where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (left, EitherT)
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
import           Servant.Docs
import           Servant.JQuery

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
userServer :: Server UserAPI
userServer = users :<|> getUser :<|> newUser

-- | The users endpoint loads the full list of users from a database,
-- which has to happen in IO, so the return type needs to incorporate
-- error conditions.
users :: EitherT ServantErr IO [User]
users = liftIO loadUsers

getUser :: Int -> EitherT ServantErr IO User
getUser theId = do
  liftIO $ putStrLn ("Javascript: "++generateJS adduserJS)

  res <- liftIO $ loadUser theId
  case res of
    Left  _err -> left userNotFound
    Right user -> return user

newUser :: User -> EitherT ServantErr IO [User]
newUser newUser = do
  res <- liftIO $ saveUser newUser
  case res of
    Left _err -> left serverError
    Right  _  -> users

usersJS :<|> userJS :<|> adduserJS = jquery userAPI

writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp functions = writeFile fp $
  concatMap generateJS functions

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] userAPI

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

docsServer _ respond =
  respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

staticServer :: Server StaticAPI
staticServer = serveDirectory "static"
          :<|> serveDirectory "static"

server :: Server FullAPI
server = userServer :<|> docsServer :<|> staticServer

app :: Application
app = serve fullAPI server
