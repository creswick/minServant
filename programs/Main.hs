-- | The main program is separated out from the actual web logic so that
-- the tests can load the web logic separately, and launch that as
-- needed.
--
module Main where

import Network.Wai.Handler.Warp (run)
import Servant

import MinServant (app)

main :: IO ()
main = run 8000 app
