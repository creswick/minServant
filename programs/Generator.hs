module Generator where

import Data.List
import System.Environment
import Servant.JQuery
import Servant
import API

-- | Generator for Javascript.
main :: IO ()
main = do
  (outFile:_) <- getArgs
  writeJS outFile [usersJS, userJS, adduserJS]

usersJS :<|> userJS :<|> adduserJS = jquery userAPI

writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp functions = writeFile fp $ unlines
  [ "'use strict';"
  , "const $ = require('jquery');"
  , ""
  , concatMap generateJS functions
  , ""
  , "module.exports = {"
  , intercalate "," $ map (\ajx-> _funcName ajx ++": "++_funcName ajx) functions
  , "};"
  ]
