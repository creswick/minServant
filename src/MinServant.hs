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
server :: Server UserAPI
server = users
    :<|> getUser
    :<|> return albert
    :<|> return isaac

isaac :: User
isaac = User 1 "Isaac Newton" 372 "isaac@newton.co.uk" "1683-3-1" -- (fromGregorian 1683 3 1)

albert :: User
albert = User 2 "Albert Einstein" 136 "ae@mc2.org" "1905-12-1" -- (fromGregorian 1905 12 1)

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

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
