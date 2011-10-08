module Rule where

import IO
import Term

import Text.Parsec
import Text.PrettyPrint.HughesPJ


data Rule = Rule { vars :: [ Identifier ] 
                 , lhs :: Term
                 , rhs :: Term
                 } 

instance Show Rule where show = render . output
instance Read Rule where readsPrec = parsec_reader

instance Output Rule where
  output r = text "forall" <+> fsep ( punctuate comma ( map output $ vars r ) 
    ++ [ text ":" , output ( lhs r ), text "->", output ( rhs r ), text "." ] )
             
instance Input Rule where             
  input = do
    string "forall" ; spaces
    vs <- sepBy input ( do string "," ; spaces )
    string ":" ; spaces
    l <- input
    string "->" ; spaces
    r <- input 
    string "." ; spaces
    return $ Rule { vars = vs, lhs = l, rhs = r }
    