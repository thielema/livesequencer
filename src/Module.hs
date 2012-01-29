module Module where

import IO ( Input, Output, input, output )
import Term ( Term, Identifier, lexer )
import Rule ( Rule )
import qualified Type
import qualified Term
import qualified Rule

import qualified Data.Map as M
import Data.Maybe ( mapMaybe )

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec ( (<|>) )
import Text.ParserCombinators.Parsec.Token ( reserved, reservedOp )
import Text.PrettyPrint.HughesPJ
           ( (<+>), ($$), empty, hsep, sep, hang, punctuate,
             render, text, vcat, parens )

import Control.Functor.HT ( void )


data Import = Import { qualified :: Bool
                     , source :: Name
                     , rename :: Maybe Identifier
                     }
--    deriving (Show)

{-
A semicolon behind an import statement is necessary when parsing

> import Prelude ;
>
> (+) :: a -> a -> a

otherwise the parentheses around the plus
would be interpreted as parentheses behind @Prelude@.
-}
instance Input Import where
    input = do
      reserved lexer "import"
      q <- Parsec.option False $ do reserved lexer "qualified" ; return True
      t <- input
      r <- Parsec.optionMaybe $ reserved lexer "as" >> input
      void $ Parsec.optionMaybe $ reserved lexer "hiding"
      void $ Parsec.optionMaybe $
          Token.parens lexer $ Token.commaSep lexer $
          (do ident <- input
              void $ Parsec.option [] $ Token.parens lexer $
                  Token.commaSep lexer $ Token.identifier lexer
              return ident)
          <|>
          Term.parenOperator
      void $ Parsec.option "" $ Token.semi lexer
      return $ Import { qualified = q, source = t, rename = r }

instance Output Import where
    output i = hsep [ text "import"
                    , if qualified i then text "qualified" else empty
                    , output $ source i
                    , case rename i of
                        Nothing -> empty
                        Just r  -> text "as" <+> output r
                    ]


data TypeSig = TypeSig [Identifier] [Term] Term
    deriving (Show)

parseIdentList :: Parsec.CharParser () [Identifier]
parseIdentList =
    Token.commaSep lexer
        (input <|> Term.parenOperator)

instance Input TypeSig where
    input = do
        names <- parseIdentList
        reservedOp lexer "::"
        context <- Type.parseContext
        typeExpr <- Type.parseExpression
        void $ Token.semi lexer
        return $ TypeSig names context typeExpr

instance Output TypeSig where
    output (TypeSig names context typeExpr) =
        hang
            (hsep ( punctuate ( text "," ) $ map output names ) <+> text "::")
            4
            (sep
                [if null context
                   then empty
                   else parens ( hsep ( punctuate ( text "," ) $
                                 map output context ) ) <+> text "=>",
                 output typeExpr <+> text ";"])


data Data = Data { lhs :: Term
                 , rhs :: [ Term ]
                 }
    deriving (Show)

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
        $$ hsep ( punctuate ( text "|" ) $ map output ( rhs d ) ) <+> text ";"


data Declaration = Rule_Declaration Rule
                 | Type_Declaration TypeSig
                 | Data_Declaration Data
    deriving (Show)

instance Input Declaration where
    input = fmap Data_Declaration input
        <|> fmap Type_Declaration (do
                names <- Parsec.try $ do
                    names <- parseIdentList
                    reservedOp lexer "::"
                    return names
                context <-
                    Parsec.try Type.parseContext
                    <|>
                    return []
                typeExpr <- Type.parseExpression
                void $ Token.semi lexer
                return $ TypeSig names context typeExpr)
        <|> fmap Rule_Declaration input

instance Output Declaration where
    output decl = case decl of
        Data_Declaration d -> output d
        Type_Declaration d -> output d
        Rule_Declaration d -> output d

-- | on module parsing:
-- identifiers contain information on their source location.
-- their sourceName (as used by Parsec) is the "show"
-- of the module name (which is an identifier).
-- So, sourceName is NOT the actual file name.
-- instead, the actual file name is kept in source_location (defined here)

data Module = Module
               { name :: Name
               , imports :: [ Import ]
               , declarations :: [ Declaration ]
               , functions :: FunctionDeclarations
               , source_text :: String
               , source_location :: FilePath
               }

newtype Name = Name {deconsName :: String}
    deriving (Eq, Ord)

instance Input Name where
    input = fmap Name Term.identifier

instance Output Name where
    output (Name n) = text n

tellName :: Name -> String
tellName (Name n) = "module " ++ n


type FunctionDeclarations = M.Map Identifier [Rule]

-- | add, or replace (if rule with exact same lhs is already present)
add_rule :: Rule -> Module -> Module
add_rule rule@(Rule.Rule ident params _rhs) m =
    m { declarations =
            update
                (\d -> case d of
                    Rule_Declaration r' ->
                        ident == Rule.name r' &&
                        params == Rule.parameters r'
                    _ -> False)
                (Rule_Declaration rule) $
            declarations m,
        functions =
            M.insertWith
                (\_ -> update ((params ==) . Rule.parameters) rule)
                ident [rule] $
            functions m }

update :: (a -> Bool) -> a -> [a] -> [a]
update matches x xs =
    let ( pre, post ) = span ( not . matches ) xs
    in  pre ++ x : drop 1 post

make_functions ::
    [Declaration] -> M.Map Identifier [Rule]
make_functions =
    M.fromListWith (flip (++)) .
    mapMaybe (\decl ->
        case decl of
            Rule_Declaration rule -> Just (Rule.name rule, [rule])
            _ -> Nothing)


{-
instance Input Module where
  input = do
-}
parse ::
    FilePath -> String ->
    Parsec.GenParser Char () Module
parse srcLoc srcText = do
    m <- Parsec.option (Name "Main") $ do
        reserved lexer "module"
        m <- input
        reserved lexer "where"
        return m
    is <- Parsec.many input
    ds <- Parsec.many input
    return $ Module {
        name = m, imports = is, declarations = ds,
        functions = make_functions ds,
        source_text = srcText,
        source_location = srcLoc }

parseUntilEOF ::
    FilePath -> String ->
    Parsec.GenParser Char () Module
parseUntilEOF srcLoc srcText = do
    m <- parse srcLoc srcText
    Parsec.eof
    return m


instance Output Module where
  output p = vcat
    [ hsep [ text "module", output $ name p, text "where" ]
    , vcat $ map output $ imports p
    , vcat $ map output $ declarations p
    ]

instance Show Module where show = render . output
-- instance Read Module where readsPrec = parsec_reader
