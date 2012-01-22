module Type where

import qualified Term
import Term ( Term(Node), Identifier(Identifier) )
import IO ( Input, input )

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec.Expr as Expr
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
           ( CharParser, Parser, (<|>), (<?>), )
import Text.ParserCombinators.Parsec.Expr
           ( Assoc(AssocRight) )

import Control.Monad.Exception.Synchronous ( Exceptional(Success,Exception) )
import Control.Monad ( liftM2 )


lexer :: T.TokenParser st
lexer =
   T.makeTokenParser $ L.emptyDef {
      L.commentStart = "{-",
      L.commentEnd = "-}",
      L.commentLine = "--",
      L.nestedComments = True,
      L.identStart = Term.identifierStart,
      L.identLetter = Term.identifierLetter,
      L.opStart = operatorStart,
      L.opLetter = operatorLetter,
      L.caseSensitive = True,
      L.reservedNames = [ "forall" ],
      L.reservedOpNames = [ "=", "::", "|" ]
      }


operators :: [[([Char], Assoc)]]
operators =
  [ [ ( "->", AssocRight ) ]
--  , [ ( ",", AssocRight) ]
  ]


parseBracket :: Parser Term
parseBracket = T.lexeme lexer $ do
    (rng,term) <-
        Term.ranged $
        Parsec.between (T.symbol lexer "[") (Parsec.char ']') parseExpression
    return (Node (Identifier { Term.name = "[]", Term.range = rng }) [term])

parseAtom :: Parser Term
parseAtom =
        T.parens lexer parseExpression
    <|> parseBracket
    <|> fmap (flip Node []) input

parseApply :: Parser Term
parseApply = do
    t <- liftM2 Term.appendArguments parseAtom $ Parsec.many parseAtom
    case t of
        Success t' -> return t'
        Exception e -> fail e


operatorStart, operatorLetter :: CharParser st Char
operatorStart  = Parsec.oneOf operatorSymbols
operatorLetter = Parsec.oneOf operatorSymbols

operatorSymbols :: [Char]
operatorSymbols = ":->"

table :: Expr.OperatorTable Char st Term
table = map ( map binary ) operators

binary :: (String, Assoc) -> Expr.Operator Char st Term
binary (s, assoc) = flip Expr.Infix assoc $ do
    rng <- Parsec.try $ T.lexeme lexer $ do
        (rng,_) <- Term.ranged $ Parsec.string s
        Parsec.notFollowedBy operatorLetter <?> ("end of " ++ show s)
        return rng
    return $ \ l r -> Node ( Identifier { Term.name = s, Term.range = rng } ) [ l, r ]


parseContext :: Parsec.GenParser Char () [Term]
parseContext = do
    constraints <-
        T.parens lexer $
            T.commaSep lexer input
    T.reservedOp lexer "=>"
    return constraints

parseExpression :: Parsec.GenParser Char () Term
parseExpression =
    Expr.buildExpressionParser table parseApply
