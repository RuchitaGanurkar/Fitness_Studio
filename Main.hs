module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Routes
import Network.Wai.Middleware
import Routes (appRoutes)

main :: IO ()
main = scotty 3000 
  appRoutes
  