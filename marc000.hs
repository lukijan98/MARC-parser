module Marc000 where
import Text.Parsec
import Data.Char
import System.Environment
import Grammar

parse_leader_0 :: Parsec String Int Leader
parse_leader_0 = do var_leader_name <- string "000"
                    var_leader_value <- count 24 (letter<|>digit<|>char ' ')
                    return (Leader var_leader_name var_leader_value)

parse_field_content_0 :: Parsec String Int Field_content
parse_field_content_0 = do modifyState (+3)
                           var_indicator <- many1 (char '_' <|> digit)
                           spaces
                           var_subfields <- many parse_subfield_0
                           modifyState (subtract 3)
                           c <- getState
                           return (Field_c_1 (Indicator (var_indicator!!0) (var_indicator!!1)) var_subfields c)

parse_control_field_0 :: Parsec String Int Field_content
parse_control_field_0  = do var_control_field_value<- many1 (noneOf "\n")
                            c <- getState
                            return (Control_Field_content var_control_field_value c)

parse_subfield_0 :: Parsec String Int Sub_Field
parse_subfield_0 = do string "|"
                      var_subfield_name <- count 1 (letter <|> digit)
                      spaces
                      var_subfield_value<-many(noneOf "\n|")
                      c <- getState
                      return (Sub_Field_1 var_subfield_name var_subfield_value c)

parse_field_0 :: Parsec String Int Field
parse_field_0 = do var_field_name <- count 3 digit
                   let number = read var_field_name :: Int
                   if number < 10 
                      then do var_control_field <- parse_control_field_0
                              choice [eof,separator_0]
                              c <- getState
                              return (Control_Field var_field_name var_control_field c)
                      else do var_field <- parse_field_content_0
                              choice [eof,separator_0]
                              c <- getState
                              return (Field_1 var_field_name var_field c)
                           

separator_0 :: Parsec String Int ()
separator_0 = spaces >> string "" >> spaces


parse_marc_0 :: Parsec String Int Marc
parse_marc_0 = do var_leader <- parse_leader_0 
                  string "\n"
                  var_fields <- try (manyTill parse_field_0 (try (lookAhead (string "000")))) <|> try (manyTill parse_field_0 eof)
                  return (Marc_1 var_leader var_fields)
