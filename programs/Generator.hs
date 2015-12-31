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
  (fp:_) <- getArgs
  writeFile fp $ unlines
    [ "'use strict';"
    , "const $ = require('jquery');"
    , ""
    , T.unpack $ jsForAPI fullAPI jquery
    , ""
    , "module.exports = {"
    , intercalate "," $ map (\fname-> fname++": "++ fname) functions
    , "};"
    ]
  where
--    functions  = []
    functions = map (T.unpack . camelCase . view funcName) (listFromAPI (Proxy :: Proxy NoTypes) fullAPI)

--  writeJS outFile fullAPI

-- usersJS :<|> userJS :<|> adduserJS = jquery userAPI

-- writeJS :: FilePath -> [AjaxReq] -> IO ()
-- writeJS fp theAPI =
  
