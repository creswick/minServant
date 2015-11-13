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

-- | The API for this web service.
-- Note that these entries *must* line up with the entries in the
-- `server` definition below.
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User
          :<|> "notAUser" :> Get '[JSON] User

-- | The set of logical handlers that go with the API above.
--
-- As mentioned above, these handlers must be listed in the same order
-- as the API entries.  If they do not, then you will (hopefully) get
-- a type error, but if you happen to swap two handlers that have the
-- same type, then the logical definitions will not match, and your
-- app will run happily -- while generating the wrong results.
server :: Server UserAPI
server = users
    :<|> return albert
    :<|> return isaac
    :<|> notAUser

isaac :: User
isaac = User 1 "Isaac Newton" 372 "isaac@newton.co.uk" "1683-3-1" -- (fromGregorian 1683 3 1)

albert :: User
albert = User 2 "Albert Einstein" 136 "ae@mc2.org" "1905-12-1" -- (fromGregorian 1905 12 1)

users :: EitherT ServantErr IO [User]
users = liftIO loadUsers

-- | Here's a user that does not exist, it always returns a 404, just
-- to show how you would do that in a handler.
-- notAUser :: User
notAUser :: EitherT ServantErr IO User
notAUser = left userNotFound

-- | A simple "user not found" 404 error.
userNotFound :: ServantErr
userNotFound = err404 { errBody = "User does not exist." }

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
