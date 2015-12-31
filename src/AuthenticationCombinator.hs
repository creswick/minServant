{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards   #-}
module AuthenticationCombinator where

import qualified Control.Exception as X
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import           Network.HTTP.Types hiding (Header) -- (status401, status403)
import           Network.Wai
import           Network.Wai.Handler.Warp -- ( run )
import           Servant
import           Servant.Server.Internal -- ( succeedWith )
import           GHC.Generics

import           Errors
import           Db

import Auth.Combinators

-- Now let's use it!

data LoginResult = LoginSuccess
                 | LoginFailure String
                   deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON LoginResult


type MyApi = "login" :> ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" String] LoginResult)
  :<|> "newuser" :> ReqBody '[FormUrlEncoded] NewUserDetails :> Post '[JSON] (Headers '[Header "Set-Cookie" String] LoginResult)
  :<|> "home" :> Get '[JSON] Int
  :<|> "secret" :> SecretApi

type SecretApi = AuthProtected :> ( "name" :> Get '[JSON] String
                               :<|> "age"  :> Get '[JSON] Int )

myApi :: Proxy MyApi
myApi = Proxy

secretServer :: Server SecretApi
secretServer = getName :<|> getAge
    where getName = return "Dread Pirate Roberts"
          getAge  = return 572

server :: Server MyApi
server = login
    :<|> newuser
    :<|> getHome
    :<|> secretServer
  where
    getHome = return 5


main :: IO ()
main = run 8090 (serve myApi server)
