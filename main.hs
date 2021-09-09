import Text.Parsec
import Grammar
import Marc000
import Marcldr
import Unimarc_standard
import Unimarc_invisible
import Data.Char
import System.Environment


document :: Parsec String Int Marcs
document = do spaces
              res <- manyTill (try parse_marc_ldr <|> try parse_marc_0 <|>try parse_marc_u_s <|> try parse_marc_u_i) eof 
              spaces
              eof
              return (Marcs res)
              
main :: IO ()
main = do (input:output:[]) <- getArgs
          cont <- readFile input
          case (runParser document 0 input cont) of
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss
             
