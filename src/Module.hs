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
import Text.ParserCombinators.Parsec.Expr
           ( Assoc(AssocLeft, AssocRight, AssocNone) )
import Text.PrettyPrint.HughesPJ
           ( (<+>), ($$), empty, hsep, sep, hang, punctuate,
             render, text, comma, vcat, parens )
import qualified Data.Char as Char

import Control.Functor.HT ( void )



nestDepth :: Int
nestDepth = 4

data Import = Import { qualified :: Bool
                     , source :: Name
                     , rename :: Maybe Identifier
                     }
--    deriving (Show)

parsePortList ::
    Parsec.GenParser Char () [Identifier]
parsePortList =
    Token.parens lexer $ flip Parsec.sepEndBy (Token.comma lexer) $
    (do ident <- input
        void $ Parsec.option [] $ Token.parens lexer $
            Token.commaSep lexer $ Token.identifier lexer
        return ident)
    <|>
    Term.parenOperator

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
      void $ Parsec.optionMaybe $ parsePortList
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
            nestDepth
            (sep
                [if null context
                   then empty
                   else parens ( hsep ( punctuate ( text "," ) $
                                 map output context ) ) <+> text "=>",
                 output typeExpr <+> text ";"])


data Data = Data { dataLhs :: Term
                 , dataRhs :: [ Term ]
                 }
    deriving (Show)

instance Input Data where
    input = do
        reserved lexer "data"
        l <- input
        reservedOp lexer "="
        rs <- Parsec.sepBy input ( reservedOp lexer "|" )
        void $ Token.semi lexer
        return $ Data { dataLhs = l, dataRhs = rs }

instance Output Data where
    output d = text "data" <+> output ( dataLhs d ) <+> text "="
        $$ hsep ( punctuate ( text "|" ) $ map output ( dataRhs d ) ) <+> text ";"


data Type = Type { typeLhs :: Term
                 , typeRhs :: Term
                 }
    deriving (Show)

instance Input Type where
    input = do
        reserved lexer "type"
        l <- input
        reservedOp lexer "="
        r <- input
        void $ Token.semi lexer
        return $ Type { typeLhs = l, typeRhs = r }

instance Output Type where
    output d =
        hang
            ( text "type" <+> output ( typeLhs d ) <+> text "=" )
            nestDepth
            ( output ( typeRhs d ) <+> text ";" )


data Infix = Infix Assoc Int [ Identifier ]

showAssoc :: Assoc -> String
showAssoc AssocLeft  = "AssocLeft"
showAssoc AssocRight = "AssocRight"
showAssoc AssocNone  = "AssocNone"

instance Show Infix where
    showsPrec p (Infix assoc prec idents) =
       showParen (p>10) $
       showString "Infix " .
       showString (showAssoc assoc) .
       showString " " .
       shows prec .
       showString " " .
       shows idents

instance Input Infix where
    input = do
        assoc <-
           Parsec.try $
           Token.lexeme lexer $
           Parsec.string "infix" >>
              ((Parsec.char 'l' >> return AssocLeft)
               <|>
               (Parsec.char 'r' >> return AssocRight)
               <|>
               return AssocNone)
        prec <-
           fmap (\c -> Char.ord c - Char.ord '0') $
           Token.lexeme lexer Parsec.digit
        ops <- Parsec.sepBy1 Term.infixOperator (Token.comma lexer)
        void $ Parsec.option "" $ Token.semi lexer
        return $ Infix assoc prec ops

instance Output Infix where
    output (Infix assoc prec idents) =
        let assocStr =
                case assoc of
                    AssocLeft  -> "l"
                    AssocRight -> "r"
                    AssocNone  -> ""
        in  hang
                (text ( "infix" ++ assocStr ) <+> text ( show prec ))
                nestDepth
                (hsep ( punctuate comma $ map output idents ) <+> text ";")


data Declaration = Type_Signature TypeSig
                 | Rule_Declaration Rule
                 | Type_Declaration Type
                 | Data_Declaration Data
                 | Infix_Declaration Infix
    deriving (Show)

instance Input Declaration where
    input = fmap Data_Declaration input
        <|> fmap Infix_Declaration input
        <|> fmap Type_Declaration input
        <|> fmap Type_Signature (do
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
        Type_Signature d -> output d
        Data_Declaration d -> output d
        Type_Declaration d -> output d
        Rule_Declaration d -> output d
        Infix_Declaration d -> output d

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
We do not define the instance Input Module,
because for proper module parsing
the caller should provide the source file path and content.

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
        void $ Parsec.optionMaybe $ parsePortList
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
