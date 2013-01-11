module Db where
import Parser
import Database.HDBC
import Database.HDBC.Sqlite3

data StockType = PORTFOLIO | STOCKS
     deriving (Eq)

createDB :: IO ()
createDB = do conn <- connectSqlite3 "stocks.db"
              run conn "CREATE TABLE portfolio (name TEXT NOT NULL, value REAL NOT NULL, dateDownloaded TEXT NOT NULL, timeDownloaded TEXT NOT NULL, chg TEXT NOT NULL, low REAL NOT NULL, high REAL NOT NULL, open REAL NOT NULL, volume REAL NOT NULL, PRIMARY KEY(name,dateDownloaded,timeDownloaded))" []
              run conn "CREATE TABLE stocks (name TEXT NOT NULL PRIMARY KEY)" []
              commit conn
              
storePortfolio :: [[Maybe String]] -> IO ()
storePortfolio [] = return ()
storePortfolio list =
     do conn <- connectSqlite3 "stocks.db"
        stmt <- prepare conn "INSERT INTO portfolio (name,value,dateDownloaded,timeDownloaded,chg,low,high,open,volume) VALUES (?,?,?,?,?,?,?,?,?)"
        sExecuteMany stmt list
        commit conn
        disconnect conn
        
storeStock :: String -> IO ()
storeStock "" = return ()
storeStock ticker =
     do conn <- connectSqlite3 "stocks.db"
        run conn "INSERT INTO stocks (name) VALUES (?)" $ [toSql (ticker::String)]
        commit conn
        disconnect conn
     
printDB :: StockType -> IO ()
printDB sType =
     do stocks <- (if sType == STOCKS then getStocks else getPortfolio)
        mapM_ putStrLn (map format stocks)

        
format :: [SqlValue] -> String
format [sqlName,sqlValue,sqlDateDownloaded,sqlTimeDownloaded,sqlChg,sqlLow,sqlHigh,sqlOpen,sqlVolume] = 
     "\n\n"++name ++ " - " ++ show value ++ " \ndata received " ++ dateDownloaded ++ "("++ timeDownloaded ++") \nChange: "++ chg ++ " Low: "++show low ++" High: "++ show high ++" \nOpen: "++ show open ++" Volume: "++ show volume
     where name = (fromSql sqlName) :: String
           value = (fromSql sqlValue)::Double
           dateDownloaded = (fromSql sqlDateDownloaded) :: String
           timeDownloaded = (fromSql sqlTimeDownloaded) :: String
           chg = (fromSql sqlChg) :: String
           low = (fromSql sqlLow) :: Double
           high = (fromSql sqlHigh) :: Double
           open = (fromSql sqlOpen) :: Double
           volume = (fromSql sqlVolume) :: Double
           
format [sqlStockName] =
     fromSql sqlStockName :: String


getPortfolio = 
     do conn <- connectSqlite3 "stocks.db"
        res <- quickQuery' conn "SELECT * FROM portfolio" []
        disconnect conn
        return res

getStocks = 
     do conn <- connectSqlite3 "stocks.db"
        res <- quickQuery' conn "SELECT * FROM stocks" []
        disconnect conn
        return res
        
makeStocksUri =
     do stocks <- getStocks
        let stocksList1D = map head stocks
        let stocksListString = (map fromSql) stocksList1D :: [String]
        let completeUri = "http://download.finance.yahoo.com/d/quotes.csv?s="++unwordsWithComma stocksListString++"&f=sl1d1t1c1ohgv&e=.csv"
        return completeUri
        
        

        


sqlFailHandler :: SqlError -> IO ()
sqlFailHandler ex = error "\nDatabase error: You already have the newest numbers, or one of your stock tickers are wrong. "
