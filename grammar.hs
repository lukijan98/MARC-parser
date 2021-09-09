module Grammar where

data Marcs = Marcs [Marc]
data Leader = Leader Leader_name Leader_value
data Marc = Marc_1 Leader [Field]
            |Marc_2 [Field]
data Field_content = Field_c_1 Indicator [Sub_Field] NumTabs 
             |Field_c_2 [Sub_Field] NumTabs
             |Control_Field_content Control_Field_value  NumTabs
data Sub_Field = Sub_Field_1 Subfield_name Subfield_value NumTabs
                 |Sub_Field_2 Subfield_value NumTabs
data Indicator = Indicator Indicator_1 Indicator_2
                    | Indicator_solo Indicator_1
data Field = Field_1 Field_name Field_content NumTabs
                     | Control_Field Control_Field_name Field_content NumTabs

type Field_name = String
type Indicator_1 = Char
type Indicator_2 = Char
type Subfield_name = String
type Subfield_value = String
type Control_Field_name = String
type Control_Field_value = String
type Leader_name = String
type Leader_value = String
type NumTabs = Int

instance Show Marcs where
    show(Marcs list) = show list

instance Show Marc where
    show (Marc_1 lider polja) = "\n{\n" ++  show lider ++ "," ++ showAttrs polja ++ "}"
        where showAttrs [] = ""
              showAttrs xs = "\n\"fields\": \n[\n" ++ attrHlpr xs ++ "]\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
    show (Marc_2 polja) = "\n{\n" ++ showAttrs polja ++ "}"
        where showAttrs [] = ""
              showAttrs xs = "\n\"fields\": \n[\n" ++ attrHlpr xs ++ "]\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
                      
instance Show Field_content where
    show (Field_c_1 indikator potpolja tabs) = ":\n" ++ "\t" ++ "\t{\n" ++ showAttrs potpolja ++ show indikator ++ "\n" ++ "\t" ++ "\t}"
        where showAttrs [] = ""
              showAttrs xs = "\t\t\t\"subfields\":\n" ++ "\t\t\t" ++ "[" ++ "\n" ++ attrHlpr xs ++ "\t\t\t],\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
    show (Control_Field_content value tabs) = ":" ++ "\"" ++ value ++ "\"\n" 
    show (Field_c_2 potpolja tabs) = ":\n" ++ "\t" ++ "\t{\n" ++ showAttrs potpolja ++ "\t\t\t" ++ "\"" ++ "ind1\":" ++ "\"" ++ "" ++ "\",\n" ++ "\t\t\t" ++ "\"" ++ "ind2\":" ++ "\"" ++ "" ++ "\"" ++ "\n" ++ "\t" ++ "\t}"
        where showAttrs [] = ""
              showAttrs xs = "\t\t\t\"subfields\":\n" ++ "\t\t\t" ++ "[" ++ "\n" ++ attrHlpr xs ++ "\t\t\t],\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
    
instance Show Leader where
    show(Leader name value) = "\"" ++  name ++ "\":" ++ "\"" ++ value ++ "\"" 
    
instance Show Sub_Field where
    show (Sub_Field_1 name value tabs) =  insertTabs tabs ++ "\t{\n" ++ insertTabs tabs ++ "\t\t\"" ++  name ++ "\": \"" ++ value ++ "\"" ++ "\n" ++ insertTabs tabs ++ "\t}"
    show (Sub_Field_2 value tabs) = insertTabs tabs ++ "\t{\n" ++ insertTabs tabs ++ "\t\t" ++  "\"" ++ value ++ "\"" ++ "\n" ++ insertTabs tabs ++ "\t}"

instance Show Indicator where
    show (Indicator name value) =  "\t\t\t" ++ "\"" ++ "ind1\":" ++ "\"" ++ [name] ++ "\",\n" ++ "\t\t\t" ++ "\"" ++ "ind2\":" ++ "\"" ++ [value] ++ "\""
    show (Indicator_solo ind) = "\t\t\t" ++ "\"" ++ "ind1\":" ++ "\"" ++ [ind] ++ "\",\n" ++ "\t\t\t" ++ "\"" ++ "ind2\":" ++ "\"" ++ "" ++ "\""
instance Show Field where
    show(Field_1 name lista tabs) =  "\t{\n\t\t" ++  show name ++ show lista ++ "\n\t}" 
    show(Control_Field name lista tabs) = "\t" ++ "{\n" ++ "\t\t" ++ show name ++ show lista ++ "\n\t" ++ "}" 

insertTabs :: Int -> String
insertTabs 0 = ""
insertTabs c = "\t" ++ insertTabs (c - 1)    
