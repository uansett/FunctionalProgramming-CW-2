module Parser where
import Text.XML.HaXml.Html.Parse

parse :: [String] -> [[String]]
parse doc = map (deleteByIndex 3) $ map  splitComma doc 

                            
-- Dirty Tricks (tm) - a tip by user "Ski" in Freenodes #Haskell channel
uncertain :: [[a]] -> [[Maybe a]]
uncertain = (map . map) Just


-- like unwords, but customised
unwordsWithComma :: [String] -> String
unwordsWithComma ws = foldr1 (\w s -> w ++ ',':s) ws

-- Makes military time from AM/PM (Not in use, currently using now() for time downloaded)
military :: String -> String
military ('1':'2':':':y:z:"am") = "00:"++[y]++[z]
military (w:x:':':y:z:"am") = [w]++[x]++[':']++[y]++[z]
military (w:':':y:z:"am") = [w]++[':']++[y]++[z]
military ('1':'2':':':y:z:"pm") = "12:"++[y]++[z]
military (w:x:':':y:z:"pm") = (show ((read ([w]++[x]) :: Int)+12)) ++ [':'] ++ [y]++[z] 
military (w:':':y:z:"pm") = (show ((read [w] :: Int)+12)) ++ [':'] ++ [y]++[z]
military a = "military conversion failed"

deleteByIndex 0 list = tail list
deleteByIndex i list = (take (i) list)++(drop (i+1) list)

-- Modified the "words" implementation (Prelude)  
splitComma :: String -> [String]                          
splitComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (==',') s'
