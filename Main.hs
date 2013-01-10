import Parser
import Db
import HttpRequest
import System.Environment
import Control.Exception


main = do args <- getArgs
          case args of
			["init"] -> createDB
			--["get"] -> getParseSave
			--["dump"] -> printAllFromDB
			--["last"] -> printLastEntry
			_ -> syntaxError
			
        
        
        
yahooData = 
     do yahoo <- getYahooData `Control.Exception.catch` yahooFailHandler
        let linesYahoo = lines yahoo
        let parsedYahoo = parse linesYahoo
        storePortfolio $ uncertain parsedYahoo --SqlException here
        
syntaxError = putStrLn "Error. \nWrite one of the following: init, get, dump, last"