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
             _ -> syntaxError
        
        
yahooData = 
     do stocksUri <- makeStocksUri
        do yahoo <- getYahooData stocksUri `Control.Exception.catch` yahooFailHandler
           let parsedYahoo = (uncertain . parse . lines) yahoo
           (storePortfolio parsedYahoo) `Control.Exception.catch` sqlFailHandler --SqlException here

syntaxError = putStrLn "Usage: \nyahoo.exe init, add <tickerName>, stocks, portfolio"