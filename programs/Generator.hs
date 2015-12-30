module Generator where

import Control.Lens (view)
import qualified Data.Text as T
import Data.List
import System.Environment
import Servant.JS
import Servant.Foreign
import Servant
import API

-- | Generator for Javascript.
main :: IO ()
main = do
  (outFile:_) <- getArgs
  writeJS outFile noteAPI

-- usersJS :<|> userJS :<|> adduserJS = jquery userAPI

-- writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp theAPI = writeFile fp $ unlines
  [ "'use strict';"
  , "const $ = require('jquery');"
  , ""
  , T.unpack $ jsForAPI theAPI jquery
  , ""
  , "module.exports = {"
  , intercalate "," $ map (\fname-> fname++": "++ fname) functions
  , "};"
  ]
  where
    functions = map (T.unpack . camelCase . view funcName) (listFromAPI (Proxy :: Proxy NoTypes) theAPI)
