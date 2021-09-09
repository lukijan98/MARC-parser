module Unimarc_standard where
import Text.Parsec
import Data.Char
import System.Environment
import Grammar

parse_field_content_u_s :: Parsec String Int Field_content
parse_field_content_u_s = do modifyState (+3)
                             spaces
                             var_indicator <- many1 (char '#' <|> digit)
                             spaces
                             var_subfields <- many parse_subfield_u_s
                             modifyState (subtract 3)
                             c <- getState
                             return (Field_c_1 (Indicator (var_indicator!!0) (var_indicator!!1)) var_subfields c)
         


parse_subfield_u_s :: Parsec String Int Sub_Field
parse_subfield_u_s = do string "["
                        var_subfield_name <- many1 (letter <|> digit)
                        string "]"
                        var_subfield_value<-many1(noneOf "\n[")
                        c <- getState
                        return (Sub_Field_1 var_subfield_name var_subfield_value c)
           

parse_field_u_s :: Parsec String Int Field
parse_field_u_s = do var_field_name <- many1 digit
                     var_field_content <- parse_field_content_u_s
                     choice[string "\n",string ""]
                     c <- getState
                     return (Field_1 var_field_name var_field_content c)

parse_fields_u_s :: Parsec String Int [Field]
parse_fields_u_s =  do var_first_field <- parse_field_u_s
                       var_rest_fields <- try (manyTill parse_field_u_s (try (lookAhead (string "001")))) <|> try (manyTill parse_field_u_s eof)
                       return (var_first_field:var_rest_fields)
                
parse_marc_u_s :: Parsec String Int Marc
parse_marc_u_s = do var_marc <- parse_fields_u_s
                    return (Marc_2 var_marc)
