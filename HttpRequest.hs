module HttpRequest where
import Network.HTTP
import Network.URI
import Data.Maybe
import Parser
import Db


        
--yahooUri = "http://download.finance.yahoo.com/d/quotes.csv?s=THIN.OL&f=sl1d1t1c1ohgv&e=.csv"
yahooUri = "http://download.finance.yahoo.com/d/quotes.csv?s=FUNCOM.OL,THIN.OL,AAPL&f=sl1d1t1c1ohgv&e=.csv"

getYahooData :: IO String
getYahooData =
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
