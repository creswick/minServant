module Main where

import Network.Wai.Handler.Warp (run)
import Servant

import MinServant (app)

main :: IO ()
main = run 8000 app
