module HttpRequest where

import Network.HTTP
import Network.URI
import Data.Maybe

rottenData :: String -> IO String
rottenData uri = 
     do res <- simpleHTTP (getRequest uri) >>= getResponseBody
        return res
