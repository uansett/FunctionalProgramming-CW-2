module Db where
import Parser
import Database.HDBC
import Database.HDBC.Sqlite3

data StockType = PORTFOLIO | INDEX
     deriving (Eq)

createDB :: IO ()
createDB = do conn <- connectSqlite3 "stocks.db"
              run conn "CREATE TABLE portfolio (name TEXT NOT NULL, value REAL NOT NULL, dateDownloaded TEXT NOT NULL, timeDownloaded TEXT NOT NULL, chg TEXT NOT NULL, low REAL NOT NULL, high REAL NOT NULL, open REAL NOT NULL, volume REAL NOT NULL, PRIMARY KEY(name,dateDownloaded,timeDownloaded))" []
              run conn "CREATE TABLE stockIndex (name TEXT NOT NULL, value REAL NOT NULL, dateDownloaded TEXT NOT NULL, timeDownloaded TEXT NOT NULL, chg TEXT NOT NULL, low REAL NOT NULL, high REAL NOT NULL, open REAL NOT NULL, volume REAL NOT NULL, PRIMARY KEY(name,dateDownloaded,timeDownloaded))" []
              commit conn
              
storePortfolio :: [[Maybe String]] -> IO ()
storePortfolio [] = return ()
storePortfolio list =
     do conn <- connectSqlite3 "stocks.db"
        stmt <- prepare conn "INSERT INTO portfolio (name,value,dateDownloaded,timeDownloaded,chg,low,high,open,volume) VALUES (?,?,?,?,?,?,?,?,?)"
        sExecuteMany stmt list
        commit conn
     
printDB :: StockType -> IO ()
printDB sType =
     do stocks <- (if sType == INDEX then getIndex else getPortfolio)
        mapM_ putStrLn (map format stocks)
        
format :: [SqlValue] -> String
format [sqlName,sqlValue,sqlDateDownloaded,sqlTimeDownloaded,sqlChg,sqlLow,sqlHigh,sqlOpen,sqlVolume] = 
     name ++ " " ++ show value ++ " " ++ dateDownloaded ++ timeDownloaded ++ chg ++ show low ++ show high ++ show open ++ show volume
     where name = (fromSql sqlName) :: String
           value = (fromSql sqlValue)::Double
           dateDownloaded = (fromSql sqlDateDownloaded) :: String
           timeDownloaded = (fromSql sqlTimeDownloaded) :: String
           chg = (fromSql sqlChg) :: String
           low = (fromSql sqlLow) :: Double
           high = (fromSql sqlHigh) :: Double
           open = (fromSql sqlOpen) :: Double
           volume = (fromSql sqlVolume) :: Double


getPortfolio = 
     do conn <- connectSqlite3 "stocks.db"
        res <- quickQuery' conn "SELECT * FROM portfolio" []
        return res

getIndex = 
     do conn <- connectSqlite3 "stocks.db"
        res <- quickQuery' conn "SELECT * FROM index" []
        return res


{-
        
storeDVDFilms :: [[Maybe String]] -> IO ()
storeDVDFilms [] = return ()
-- Format: storeDVDFilms [[Just "99",Just "The Hobbit"]]
storeDVDFilms xs = 
     do conn <- connectSqlite3 "films.db"
        stmt <- prepare conn "INSERT INTO dvd (votePct,name,dateAdded) VALUES (?,?,date('now'))"
        sExecuteMany stmt (xs)
        commit conn
        
printFilms :: FilmType -> IO ()
printFilms x = 
     do films <- (if x == DVD then getDVDFilms else getCINEMAFilms)
        mapM_ putStrLn (map fromSql' films)
        
     
fromSql' :: [SqlValue] -> String
fromSql' [sqlVotePct, sqlName, sqlDateAdded] =
     name ++", voted "++ show votePct++"%"
     where votePct = (fromSql sqlVotePct)::Integer
           name = (fromSql sqlName)::String
           
fromSql' [sqlVotePct, sqlName, sqlValueInM, sqlDateAdded] =
     name ++",voted "++ show votePct++"%"
     where votePct = (fromSql sqlVotePct)::Integer
           name = (fromSql sqlName)::String
fromSql' x = fail $ "Unexpected result: "++ show x
        
getDVDFilms :: IO [[SqlValue]]
getDVDFilms = 
     do conn <- connectSqlite3 "films.db"
        res <- quickQuery' conn "SELECT votePct,name,dateAdded FROM dvd" []
        return res

getCINEMAFilms :: IO [[SqlValue]]
getCINEMAFilms =
     do conn <- connectSqlite3 "films.db"
        res <- quickQuery' conn "SELECT votePct,name,valueInM,dateAdded FROM cinema" []
        return res
     
		  
-}