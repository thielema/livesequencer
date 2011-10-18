module Module where

import IO
import Term ( Identifier, lexer )
import Rule

import Text.Parsec
import Text.Parsec.Token
import Text.PrettyPrint.HughesPJ
{-
import qualified Data.Set as S
import Control.Monad ( guard )
-}

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

data Module = Module
               { name :: Maybe Identifier
               , imports :: [ Import ]
               , rules :: [ Rule ]
               }


instance Input Module where
  input = do
    m <- optionMaybe $ do
        reserved lexer "module" 
        m <- input
        reserved lexer "where"
        return m
    is <- many input
    rs <- many input
    return $ Module { name = m , imports = is , rules = rs }

instance Output Module where
  output p = vcat 
    [ case name p of
       Nothing -> empty
       Just m  -> hsep [ text "module", output m, text "where" ]
    , vcat $ map output $ imports p
    , vcat $ map output $ rules p
    ]  

instance Show Module where show = render . output
instance Read Module where readsPrec = parsec_reader
