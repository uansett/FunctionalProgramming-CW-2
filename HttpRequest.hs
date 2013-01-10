module HttpRequest where
import Control.Exception
import Network.HTTP
import Network.URI
import Data.Maybe
import Parser

rottenData = 
     do rotten <- getRottenData `Control.Exception.catch` rottenFailHandler
        let parsedRotten = parse rotten
        print parsedRotten
        
rottenUri = "http://download.finance.yahoo.com/d/quotes.csv?s=THIN.OL&f=sl1d1t1c1ohgv&e=.csv"

getRottenData :: IO String
getRottenData =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ "Error connecting: " ++ show x
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ rspBody r
               _ -> return $ show r
    where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
          uri = fromJust $ parseURI rottenUri
        
rottenFailHandler :: IOError -> IO String
rottenFailHandler ex = error "\nError downloading Rotten Tomatoes data. Something is rotten here.."
