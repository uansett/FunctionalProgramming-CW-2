import Parser
import Db
import HttpRequest
import System.Environment

main = do args <- getArgs
          case args of
			["init"] -> createDB
			--["get"] -> getParseSave
			--["dump"] -> printAllFromDB
			--["last"] -> printLastEntry
			_ -> syntaxError
			
syntaxError = putStrLn "Error. \nWrite one of the following: init, get, dump, last"