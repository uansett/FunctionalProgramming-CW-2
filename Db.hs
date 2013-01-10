module Db where
import Parser
import HttpRequest
import Database.HDBC
import Database.HDBC.Sqlite3

createDB :: IO ()
createDB = do conn <- connectSqlite3 "stocks.db"
              run conn "CREATE TABLE portfolio (name TEXT, value REAL, dateDownloaded TEXT, timeDownloaded TEXT, chg TEXT, low REAL, high REAL, open REAL, volume REAL)"














{-
data FilmType = DVD | CINEMA | ALL -- OBS OBS OBS OBS OBS OBS ALLE
     deriving Eq


createDB :: IO ()
createDB = do conn <- connectSqlite3 "films.db"
              run conn "CREATE TABLE cinema (votePct INTEGER, name TEXT, valueInM INTEGER, dateAdded TEXT)" []
              run conn "CREATE TABLE dvd (votePct INTEGER, name TEXT, dateAdded TEXT)" []
              commit conn
			  				
storeCinemaFilms :: [[Maybe String]] -> IO ()
storeCinemaFilms [] = return ()
-- Format: storeCinemaFilms [[Just "99",Just "The Hobbit", Just "34"]]
storeCinemaFilms xs = 
     do conn <- connectSqlite3 "films.db"
        stmt <- prepare conn "INSERT INTO cinema (votePct,name,valueInM,dateAdded) VALUES (?,?,?,date('now'))"
        sExecuteMany stmt (xs)
        commit conn
        
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