module Rule where

import IO
import Term ( Term(Node), Identifier )
import qualified Term

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.PrettyPrint.HughesPJ ( fsep, render, text )

import Control.Monad ( when )


data Rule = Rule
    { name :: Identifier
    , parameters :: [ Term ]
    , rhs :: Term
    }

instance Show Rule where show = render . output
instance Read Rule where readsPrec = parsec_reader

instance Output Rule where
  output r =
    fsep [ output ( Node (name r) (parameters r) ), text "=",
           output ( rhs r ), text ";" ]

instance Input Rule where
  input = do
    nm <- input
    when (not $ Term.isVariable nm)
        (fail $ show nm ++ " is not a function identifier")
    ps <- Parsec.many (Term.parse True)
    Term.symbol "="
    r <- input
    Term.symbol ";"
    return $ Rule { name = nm, parameters = ps, rhs = r }
