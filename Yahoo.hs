{-
=====================================================
     Y A H O O  P O R T F O L I O  F E T C H E R
=====================================================
Author: Stian A. Johansen
Does:   Downloads the most recent numbers for a set 
        of tickers (names of stocks) found in the
        "stocks" table from Yahoo Finance in csv
        format, and inserts it into table "portfolio".
        
        The code is based on the sample code given
        for this coursework, and the sourcecode of 
        some already defined modules in the 
        Prelude library. 


-}


import Parser
import Db
import HttpRequest
import System.Environment
import Control.Exception

main = do args <- getArgs
          case args of
             ["init"] -> createDB
             ["stocks"] -> printDB STOCKS
             ["portfolio"] -> printDB PORTFOLIO
             ["add", ticker] -> storeStock ticker
             ["download"] -> yahooData
             ["active"] -> printDB CHANGE
             _ -> syntaxError
        
        
yahooData = 
     do stocksUri <- makeStocksUri
        do yahoo <- getYahooData stocksUri `Control.Exception.catch` yahooFailHandler
           let parsedYahoo = (uncertain . parse . lines) yahoo
           (storePortfolio parsedYahoo) `Control.Exception.catch` sqlFailHandler --SqlException here

syntaxError = putStrLn "Usage: \nYahoo.exe (argument [argument])\ninit - creates the Database\nstocks - prints the stock ticker names in the database\nportfolio - prints the most recent numbers for your stocks\nadd <tickerName> - adds a stock to the database\ndownload - gets fresh data from Yahoo! if there is any newer than already in the database.\nactive - prints the stock ticker with the biggest change for the day the data was downloaded."