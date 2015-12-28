{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
module AuthenticationCombinator where

import           Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString
import Network.HTTP.Types hiding (Header) -- (status401, status403)
import Network.Wai
import Network.Wai.Handler.Warp -- ( run )
import Servant
import Servant.Server.Internal -- ( succeedWith )

-- import           Network.Wai
-- import           Network.Wai.Handler.Warp

import GHC.Generics

-- import           Control.Monad.Trans.Either (left, EitherT)
import           Control.Monad.Trans.Except
import qualified Control.Exception as X

import Errors
import Db

type DBLookup = ByteString -> IO Bool

isGoodCookie :: DBLookup
isGoodCookie = return . (== "myHardcodedCookie")

data AuthProtected

instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) $ addAcceptCheck subserver $ cookieCheck request
      where
        cookieCheck req = case lookup "Cookie" (requestHeaders req) of
            Nothing -> return $ FailFatal err401 { errBody = "Missing auth header" }
            Just v  -> do
              authGranted <- isGoodCookie v
              if authGranted
                then return $ Route ()
                else return $ FailFatal err403 { errBody = "Invalid cookie" }

-- Now let's use it!
data Credentials = Credentials { username :: String
                               , password :: String
                               } deriving (Read, Show, Eq, Ord, Generic)

instance FromFormUrlEncoded Credentials

data LoginResult = LoginSuccess
                 | LoginFailure String
                   deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON LoginResult

type AuthAPI = "login" :> ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] LoginResult

type MyApi = "login" :> ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" String] LoginResult)
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
    :<|> getHome
    :<|> secretServer
  where getHome = return 5

login :: Credentials -> ExceptT ServantErr IO (Headers '[Header "Set-Cookie" String] LoginResult)
login cr@(Credentials name pass) | pass == "please" = doLogin cr
                                 | otherwise        = throwE userNotFound -- TODO wrong response.

-- doLogin :: Credentials -> EitherT ServantErr IO LoginResult
doLogin (Credentials name _) = do
  let userid = 2 -- TODO use a DB lookup.
  eSessionCookie <- liftIO $ newSession userid
  case eSessionCookie of
    Left            err -> throwE $ err403 { errBody = "Invalid cookie" }
    Right sessionCookie -> return $ addHeader sessionCookie LoginSuccess

main :: IO ()
main = run 8090 (serve myApi server)
