module Parser where
import Text.XML.HaXml.Html.Parse


parse :: [String] -> [[String]]
parse doc = map splitComma doc
                            
                            

uncertain = (map . map) Just

-- Modified the "words" implementation (Prelude)  
splitComma :: String -> [String]                          
splitComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (==',') s'