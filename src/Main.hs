{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant

main :: IO ()
main = run 8000 app

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users :: [User]
users = [isaac, albert]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return isaac
    :<|> return users
    :<|> return albert

app :: Application
app = serve userAPI server
