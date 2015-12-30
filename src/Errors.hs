{-# LANGUAGE OverloadedStrings #-}
module Errors
 ( userNotFound
 , noteNotFound
 , fileNotFound
 , serverError
 ) where

import qualified Data.ByteString.Lazy.Char8 as LC8

import Servant

-- | A simple "note not found" 404 error.
noteNotFound :: ServantErr
noteNotFound = err404 { errBody = "Note does not exist." }

userNotFound :: ServantErr
userNotFound = err404 { errBody = "User does not exist." }

-- | File not found 404 error.
fileNotFound :: FilePath -> ServantErr
fileNotFound filepath = err404 { errBody = LC8.pack ("File not found: " ++ filepath) }

-- | Basic server error.
serverError :: ServantErr
serverError = err500 { errBody = "Server error." }
