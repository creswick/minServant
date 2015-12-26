{-# LANGUAGE OverloadedStrings #-}
module Errors
 ( userNotFound
 , fileNotFound
 , serverError
 ) where

import qualified Data.ByteString.Lazy.Char8 as LC8

import Servant

-- | A simple "user not found" 404 error.
userNotFound :: ServantErr
userNotFound = err404 { errBody = "User does not exist." }

-- | File not found 404 error.
fileNotFound :: FilePath -> ServantErr
fileNotFound filepath = err404 { errBody = LC8.pack ("File not found: " ++ filepath) }

-- | Basic server error.
serverError :: ServantErr
serverError = err500 { errBody = "Server error." }
