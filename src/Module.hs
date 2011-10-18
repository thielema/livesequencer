module Module where

import IO
import Term ( Identifier, lexer )
import Rule

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


-- | on module parsing:
-- identifiers contain information on their source location.
-- their sourceName (as used by Parsec) is the "show" 
-- of the module name (which is an identifier).
-- So, sourceName is NOT the actual file name.
-- instead, this the actual file name is kept in source_location (defined here)

data Module = Module
               { name :: Identifier
               , imports :: [ Import ]
               , rules :: [ Rule ]
               , source_text :: String
               , source_location :: FilePath
               }


instance Input Module where
  input = do
    m <- option ( read "Main" ) $ do
        reserved lexer "module" 
        m <- input
        reserved lexer "where"
        return m
    is <- many input
    rs <- many input
    return $ Module { name = m , imports = is , rules = rs }

instance Output Module where
  output p = vcat 
    [ hsep [ text "module", output $ name p, text "where" ]
    , vcat $ map output $ imports p
    , vcat $ map output $ rules p
    ]  

instance Show Module where show = render . output
instance Read Module where readsPrec = parsec_reader
