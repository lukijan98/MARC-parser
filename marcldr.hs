module Marcldr where
import Control.Monad
import Text.Parsec
import Data.Char
import System.Environment
import Grammar

parse_leader_ldr :: Parsec String Int Leader
parse_leader_ldr = do var_leader_name <- try(string "LDR") <|> (string "LEADER")
                      spaces
                      var_leader_value <- count 24 (letter<|>digit<|>char '*' <|> space)
                      return (Leader var_leader_name var_leader_value)

parse_field_content_ldr :: Parsec String Int Field_content
parse_field_content_ldr = do modifyState (+3)
                             spaces
                             var_indicator <- many (char '#' <|> digit)
                             spaces
                             var_subfields <- many parse_subfield_ldr
                             modifyState (subtract 3)
                             c <- getState
                             let var_indicator_length = (length var_indicator)
                             if (var_indicator_length) < 2
                                 then if (var_indicator_length) == 0
                                          then return(Field_c_2 var_subfields c)
                                          else return (Field_c_1 (Indicator_solo (var_indicator!!0)) var_subfields c)
                                 else return (Field_c_1 (Indicator (var_indicator!!0) (var_indicator!!1)) var_subfields c)

parse_control_field_ldr  :: Parsec String Int Field_content
parse_control_field_ldr  = do spaces
                              var_control_field_value <- many1 (noneOf "\n")
                              c <- getState
                              return (Control_Field_content var_control_field_value c)

parse_subfield_ldr :: Parsec String Int Sub_Field
parse_subfield_ldr = do string "$"
                        var_subfield_name <- count 1 (letter <|> digit)
                        var_subfield_value <-many(noneOf "\n$")
                        c <- getState
                        return (Sub_Field_1 var_subfield_name var_subfield_value c)

parse_field_ldr :: Parsec String Int Field
parse_field_ldr = do var_field_name <- count 3 digit
                     let number = read var_field_name :: Int
                     if number < 10 
                        then do var_control_field <- parse_control_field_ldr
                                choice [eof,separator_ldr]
                                c <- getState
                                return (Control_Field var_field_name var_control_field c)
                        else do var_field <- parse_field_content_ldr
                                choice [eof,separator_ldr]
                                c <- getState
                                return (Field_1 var_field_name var_field c)
                           

separator_ldr :: Parsec String Int ()
separator_ldr = spaces >> string "" >> spaces


parse_marc_ldr :: Parsec String Int Marc
parse_marc_ldr = do var_leader <- parse_leader_ldr
                    string "\n"
                    var_fields <- try (manyTill parse_field_ldr (lookAhead (string "LDR"))) <|> try (manyTill parse_field_ldr (lookAhead (string "LEADER"))) <|> try (manyTill parse_field_ldr eof)
                    return (Marc_1 var_leader var_fields)
