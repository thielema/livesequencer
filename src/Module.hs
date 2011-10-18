module Module where

import IO
import Term ( Term, Identifier, lexer )
import Rule ( Rule )

import Text.Parsec
import Text.Parsec.Token
import Text.PrettyPrint.HughesPJ

data Import = Import { qualified :: Bool
                     , source :: Identifier  
                     , rename :: Maybe Identifier  
                     }  
              
instance Input Import where
    input = do
      reserved lexer "import"
      q <- option False $ do reserved lexer "qualified" ; return True
      t <- input
      r <- optionMaybe $ do reserved lexer "as" ; input
      return $ Import { qualified = q, source = t, rename = r }
      
instance Output Import where      
    output i = hsep [ text "import"
                    , if qualified i then text "qualified" else  empty
                    , output $ source i
                    , case rename i of  
                        Nothing -> empty
                        Just r  -> text "as" <+> output r
                    ]    

data Data = Data { lhs :: Term 
                 , rhs :: [ Term ]  
                 }  

instance Input Data where            
    input = do
        reserved lexer "data"
        l <- input  
        reservedOp lexer "="
        rs <- sepBy input ( reservedOp lexer "|" ) 
        Text.Parsec.Token.semi lexer
        return $ Data { lhs = l, rhs = rs }

instance Output Data where        
    output d = text "data" <+> output ( lhs d ) <+> text "="
        $$ hsep ( punctuate ( text "|") $ map output ( rhs d ) ) <+> text ";"

data Type 

data Declaration = Rule_Declaration Rule
                 | Type_Declaration Type  
                 | Data_Declaration Data  

instance Input Declaration where
    input = do d <- input ; return $ Data_Declaration d
        <|> do r <- input ; return $ Rule_Declaration r        

instance Output Declaration where
    output d = case d of
        Data_Declaration d -> output d
        Rule_Declaration d -> output d

-- | on module parsing:
-- identifiers contain information on their source location.
-- their sourceName (as used by Parsec) is the "show" 
-- of the module name (which is an identifier).
-- So, sourceName is NOT the actual file name.
-- instead, this the actual file name is kept in source_location (defined here)

data Module = Module
               { name :: Identifier
               , imports :: [ Import ]
               , declarations :: [ Declaration ]
               , source_text :: String
               , source_location :: FilePath
               }

rules m = do
    Rule_Declaration r <- declarations m
    return r

instance Input Module where
  input = do
    m <- option ( read "Main" ) $ do
        reserved lexer "module" 
        m <- input
        reserved lexer "where"
        return m
    is <- many input
    ds <- many input
    return $ Module { name = m , imports = is , declarations = ds }

instance Output Module where
  output p = vcat 
    [ hsep [ text "module", output $ name p, text "where" ]
    , vcat $ map output $ imports p
    , vcat $ map output $ declarations p
    ]  

instance Show Module where show = render . output
instance Read Module where readsPrec = parsec_reader
