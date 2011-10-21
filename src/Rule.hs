module Rule where

import IO
import Term

import Text.PrettyPrint.HughesPJ ( fsep, render, text )


data Rule = Rule { lhs :: Term
                 , rhs :: Term
                 }

instance Show Rule where show = render . output
instance Read Rule where readsPrec = parsec_reader

instance Output Rule where
  output r = fsep [ output ( lhs r ), text "=", output ( rhs r ), text ";" ]

instance Input Rule where
  input = do
    l <- input
    symbol "="
    r <- input
    symbol ";"
    return $ Rule { lhs = l, rhs = r }
