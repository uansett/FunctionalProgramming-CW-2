module Parser where
import Text.XML.HaXml.Html.Parse


parse :: [String] -> [[String]]
parse doc = map splitComma doc
                            
                            
-- Dirty Tricks (tm)
uncertain = (map . map) Just
-- like unwords, but customised
unwordsWithComma ws = foldr1 (\w s -> w ++ ',':s) ws

-- Modified the "words" implementation (Prelude)  
splitComma :: String -> [String]                          
splitComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (==',') s'