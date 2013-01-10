module Parser where
import Text.XML.HaXml.Html.Parse


parse :: String -> [String]
parse doc = split (==',') doc
                              
split separator doc =  case dropWhile separator doc of
                      "" -> []
                      doc' -> w : split separator doc''
                            where (w, doc'') = break separator doc'