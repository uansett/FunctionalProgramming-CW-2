module Parser where
import HttpRequest
--test temp
doItAll = extractMovieList (rottenData "http://www.rottentomatoes.com")

extractMovieList document = document >>= print (take 100)
     
     