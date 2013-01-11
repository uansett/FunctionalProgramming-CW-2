module HttpRequest where
import Network.HTTP
import Network.URI
import Data.Maybe
import Parser
import Db


getYahooData :: String -> IO String
getYahooData yahooUri =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ "Error connecting: " ++ show x
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ rspBody r
               _ -> return $ show r
    where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
          uri = fromJust $ parseURI yahooUri
        
yahooFailHandler :: IOError -> IO String
yahooFailHandler ex = error "\nError downloading Yahoo data. Are you online?"
