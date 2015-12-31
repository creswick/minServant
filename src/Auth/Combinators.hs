{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Authentication combinators, adapted from servant-examples:
-- https://github.com/haskell-servant/servant/tree/master/servant-examples/auth-combinator
module Auth.Combinators where

import Data.ByteString (ByteString)

import qualified Control.Exception as X
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import           Network.HTTP.Types hiding (Header) -- (status401, status403)
import           Network.Wai
import           Network.Wai.Handler.Warp -- ( run )
import           Servant
import           Servant.Server.Internal -- ( succeedWith )
import           GHC.Generics

import Db
import Errors

type DBLookup = String -> IO Bool

-- | Predicate to do db lookups and confirm sessions cookie validity.
isGoodCookie :: DBLookup
isGoodCookie sessionCookie = do
  res <- validSessionCookie sessionCookie
  return res

-- | Type combinator to protect specified APIs from users who do not
-- have valid sessions.
--
-- To use:
--
-- > type SecretAPI = AuthProtected :> ("name" :> Get '[JSON] String
-- >                                :<|> "age"  :> Get '[JSON] Int )
-- > type MyAPI = "public" :> Get '[JSON] String
-- >         :<|> "auth" :> authAPI
-- >         :<|> "protected" :> SecretAPI
--
-- Note that the protected api will have a longer path (e.g., "protected/name")
--
-- Then create the server as you normally would:
--
-- > secretServer :: Server SecretAPI
-- > secretServer = getName :<|> getAge
-- >
-- > server :: Server MyAPI
-- > server = public
-- >     :<|> authServer
-- >     :<|> secretServer
--
data AuthProtected

instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) $ addAcceptCheck subserver $ cookieCheck request
      where
        cookieCheck req = case lookup "Cookie" (requestHeaders req) of
            Nothing -> return $ FailFatal err401 { errBody = "Missing auth header" }
            Just v  -> do
              authGranted <- isGoodCookie $ C8.unpack v
              if authGranted
                then return $ Route ()
                else return $ FailFatal err403 { errBody = "Invalid cookie" }

type AuthAPI = "login" :>  ReqBody '[FormUrlEncoded] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" String] ())
          :<|> "newuser" :> ReqBody '[FormUrlEncoded] NewUserDetails :> Post '[JSON] (Headers '[Header "Set-Cookie" String] ())
          :<|> "logout" :> Header "Cookie" String :> Get '[JSON] ()
          :<|> "loggedin" :> Header "Cookie" String :> Get '[JSON] Bool


authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer :: Server AuthAPI
authServer = login
        :<|> newuser
        :<|> logout
        :<|> loggedIn

data Credentials = Credentials { username :: String
                               , password :: String
                               } deriving (Read, Show, Eq, Ord, Generic)

instance FromFormUrlEncoded Credentials where
  fromFormUrlEncoded theMap = do
    username <- T.unpack `fmap` lookupEither "Could not find username" "username" theMap
    password <- T.unpack `fmap` lookupEither "Could not find password" "password" theMap
    return Credentials {..}

-- | Wrapper around `lookup` that returns an either, with a provided failure message.
lookupEither :: Eq a => String -> a -> [(a, b)] -> Either String b
lookupEither err key map = case lookup key map of
  Nothing -> Left err
  Just  v -> Right v

data NewUserDetails = NewUserDetails { nudUsername :: String
                                     , nudPassword :: String
                                     , nudEmail :: String
                                     } deriving (Read, Show, Eq, Ord, Generic)

instance FromFormUrlEncoded NewUserDetails where
  fromFormUrlEncoded theMap = do
    nudUsername <- T.unpack `fmap` lookupEither "Could not find username" "username" theMap
    nudPassword <- T.unpack `fmap` lookupEither "Could not find password" "password" theMap
    nudEmail <- T.unpack `fmap` lookupEither "Could not find email" "email" theMap
    return NewUserDetails {..}

login :: Credentials -> ExceptT ServantErr IO (Headers '[Header "Set-Cookie" String] ())
login cr@(Credentials name pass) | pass == "please" = doLogin cr
                                 | otherwise        = throwE userNotFound -- TODO wrong response.

doLogin :: Credentials -> ExceptT ServantErr IO (Headers '[Header "Set-Cookie" String] ())
doLogin (Credentials name _) = do
  let userid = 2 -- TODO use a DB lookup.
  eSessionCookie <- liftIO $ newSession userid
  case eSessionCookie of
    Left            err -> throwE $ err403 { errBody = LC8.pack ("Could not validate session cookie: "++ err) }
    Right sessionCookie -> return $ addHeader sessionCookie ()

loggedIn :: Maybe String -> ExceptT ServantErr IO Bool
loggedIn Nothing              = return False
loggedIn (Just sessionCookie) = do
  liftIO $ validSessionCookie sessionCookie

-- | Create a new user account and log the user in.
newuser :: NewUserDetails -> ExceptT ServantErr IO (Headers '[Header "Set-Cookie" String] ())
newuser nud = do
  -- TODO create new user in db:
  eRes <- liftIO $ newUser (nudUsername nud) (nudPassword nud) (nudEmail nud)
  case eRes of
    Left err -> do liftIO $ putStrLn ("Could not create new user: "++show nud++" error: "++show err)
                   throwE $ err403 { errBody = LC8.pack "Could not create new user." }
    Right () -> doLogin Credentials { username = nudUsername nud
                                    , password = nudPassword nud
                                    }

-- | Invalidate the current session cookie.
logout :: Maybe String -> ExceptT ServantErr IO ()
logout Nothing       = return ()
logout (Just cookie) = do
  eRes <- liftIO $ clearSessionCookie cookie
  case eRes of
    Left err -> liftIO $ putStrLn ("Error clearing cookie: "++show err)
    Right () -> return ()
