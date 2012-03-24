module Rule where

import IO ( Input(input), Output(output), parsecReader )
import Term ( Term(Node), Identifier )
import qualified Term

import Text.PrettyPrint.HughesPJ ( fsep, render, text )


data Rule = Rule
    { name :: Identifier
    , parameters :: [ Term ]
    , rhs :: Term
    }

instance Show Rule where show = render . output
instance Read Rule where readsPrec = parsecReader

instance Output Rule where
  output r =
    fsep [ output ( Node (name r) (parameters r) ), text "=",
           output ( rhs r ), text ";" ]

instance Input Rule where
  input = do
    t <- input
    (nm, ps) <-
        case t of
            Term.Node nm args ->
                if Term.isVariable nm
                  then return (nm, args)
                  else fail $ show nm ++ " is not a function identifier"
            _ -> fail $ "the term " ++ show t ++ " is not a valid left-hand side of a rule"
    Term.symbol "="
    r <- input
    Term.symbol ";"
    return $ Rule { name = nm, parameters = ps, rhs = r }
