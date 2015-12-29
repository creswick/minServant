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
import           Data.ByteString
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

type DBLookup = ByteString -> IO Bool

isGoodCookie :: DBLookup
isGoodCookie sessionCookie = do
  res <- validSessionCookie sessionCookie
  return res

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

instance FromFormUrlEncoded Credentials where
  fromFormUrlEncoded theMap = do
    username <- T.unpack `fmap` lookupEither "Could not find username" "username" theMap
    password <- T.unpack `fmap` lookupEither "Could not find password" "password" theMap
    return Credentials {..}

lookupEither :: Eq a => String -> a -> [(a, b)] -> Either String b
lookupEither err key map = case lookup key map of
  Nothing -> Left err
  Just  v -> Right v

data LoginResult = LoginSuccess
                 | LoginFailure String
                   deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON LoginResult


type MyApi = "login" :> ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" String] LoginResult)
        :<|> "home" :> Get '[JSON] Int
        :<|> "secret" :> SecretApi
        :<|> "catchall" :> Get '[JSON] Int

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
    :<|> catchall
  where
    getHome = return 5
    catchall = return 42

login :: Credentials -> ExceptT ServantErr IO (Headers '[Header "Set-Cookie" String] LoginResult)
login cr@(Credentials name pass) | pass == "please" = doLogin cr
                                 | otherwise        = throwE userNotFound -- TODO wrong response.

-- doLogin :: Credentials -> EitherT ServantErr IO LoginResult
doLogin (Credentials name _) = do
  let userid = 2 -- TODO use a DB lookup.
  eSessionCookie <- liftIO $ newSession userid
  case eSessionCookie of
    Left            err -> throwE $ err403 { errBody = LC8.pack ("Could not validate session cookie: "++ err) }
    Right sessionCookie -> return $ addHeader sessionCookie LoginSuccess

main :: IO ()
main = run 8090 (serve myApi server)
