module Generator where

import Control.Lens (view)
import qualified Data.Text as T
import Data.List
import System.Environment
import Servant.JS
import Servant.Foreign
import Servant
import API
import Auth

-- | Generator for Javascript.
main :: IO ()
main = do
  let theAPI = clientAPI
  (fp:_) <- getArgs
  writeFile fp $ unlines
    [ "'use strict';"
    , "const $ = require('jquery');"
    , ""
    , T.unpack $ jsForAPI theAPI jquery
    , ""
    , "module.exports = {"
    , intercalate ",\n" $ map (\fname-> fname++": "++ fname) (functions theAPI)
    , "};"
    ]
  where
--    functions  = []
    functions theAPI = map (T.unpack . camelCase . view funcName) (listFromAPI (Proxy :: Proxy NoTypes) theAPI)

--  writeJS outFile fullAPI

-- usersJS :<|> userJS :<|> adduserJS = jquery userAPI

-- writeJS :: FilePath -> [AjaxReq] -> IO ()
-- writeJS fp theAPI =
  
