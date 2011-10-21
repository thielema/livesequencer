{-# language EmptyDataDecls #-}

module Module where

import IO
import Term ( Term, Identifier, lexer )
import Rule ( Rule )

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec ( (<|>) )
import Text.ParserCombinators.Parsec.Token ( reserved, reservedOp )
import Text.PrettyPrint.HughesPJ ((<+>), ($$), empty, hsep, punctuate, render, text, vcat )

import Common ( void )


data Import = Import { qualified :: Bool
                     , source :: Identifier
                     , rename :: Maybe Identifier
                     }

instance Input Import where
    input = do
      reserved lexer "import"
      q <- Parsec.option False $ do reserved lexer "qualified" ; return True
      t <- input
      r <- Parsec.optionMaybe $ reserved lexer "as" >> input
      void $ Parsec.optionMaybe $ reserved lexer "hiding"
      void $ Parsec.optionMaybe $
          Token.parens lexer $ Token.commaSep lexer $
          (do ident <- Token.identifier lexer
              void $ Parsec.option [] $ Token.parens lexer $
                  Token.commaSep lexer $ Token.identifier lexer
              return ident) <|>
          Token.parens lexer (Token.operator lexer)
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
        rs <- Parsec.sepBy input ( reservedOp lexer "|" )
        void $ Token.semi lexer
        return $ Data { lhs = l, rhs = rs }

instance Output Data where
    output d = text "data" <+> output ( lhs d ) <+> text "="
        $$ hsep ( punctuate ( text "|") $ map output ( rhs d ) ) <+> text ";"

data Type

data Declaration = Rule_Declaration Rule
                 | Type_Declaration Type
                 | Data_Declaration Data

instance Input Declaration where
    input = do fmap Data_Declaration input
        <|> do fmap Rule_Declaration input

instance Output Declaration where
    output decl = case decl of
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

rules :: Module -> [Rule]
rules m = do
    Rule_Declaration r <- declarations m
    return r

instance Input Module where
  input = do
    m <- Parsec.option ( read "Main" ) $ do
        reserved lexer "module"
        m <- input
        reserved lexer "where"
        return m
    is <- Parsec.many input
    ds <- Parsec.many input
    return $ Module { name = m , imports = is , declarations = ds }

instance Output Module where
  output p = vcat
    [ hsep [ text "module", output $ name p, text "where" ]
    , vcat $ map output $ imports p
    , vcat $ map output $ declarations p
    ]

instance Show Module where show = render . output
instance Read Module where readsPrec = parsec_reader
