import Parser
import Db
import HttpRequest
import System.Environment
import Control.Exception


main = do args <- getArgs
          case args of
			["init"] -> createDB
			["add", ticker] -> storeStock ticker -- NATT
			["stocks"] -> printDB STOCKS
			["portfolio"] -> printDB PORTFOLIO
			_ -> syntaxError
			
        
        
        
yahooData = 
     do stocksUri <- makeStocksUri
        do yahoo <- getYahooData stocksUri `Control.Exception.catch` yahooFailHandler
           let parsedYahoo = (uncertain . parse . lines) yahoo
           (storePortfolio parsedYahoo) `Control.Exception.catch` sqlFailHandler --SqlException here

syntaxError = putStrLn "Usage: \nyahoo.exe init, add <tickerName>, stocks, portfolio"