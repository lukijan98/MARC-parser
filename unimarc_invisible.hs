module Unimarc_invisible where
import Text.Parsec
import Data.Char
import System.Environment
import Grammar

parse_field_content_u_i :: Parsec String Int Field_content
parse_field_content_u_i = do modifyState (+3)
                             var_indicator <- many1 (digit <|> space)
                             var_subfields <- many parse_subfield_u_i
                             modifyState (subtract 3)
                             c <- getState
                             return (Field_c_1 (Indicator (var_indicator!!0) (var_indicator!!1)) var_subfields c)
         


parse_subfield_u_i :: Parsec String Int Sub_Field
parse_subfield_u_i = do char (chr 31)
                        var_subfield_value <- many1 (noneOf (['\n']++[chr 31] ++ [chr 30]))
                        choice[string [chr 30],string ""]
                        c <- getState
                        return (Sub_Field_2 var_subfield_value c)
           

parse_fields_u_i :: Parsec String Int [Field]
parse_fields_u_i = many $ do var_field_name <- count 3 digit
                             var_field_content <- parse_field_content_u_i
                             c <- getState
                             return (Field_1 var_field_name var_field_content c)
                
parse_marc_u_i :: Parsec String Int Marc
parse_marc_u_i = do var_marc <- parse_fields_u_i
                    choice[string "\n",string ""]
                    return (Marc_2 var_marc)
