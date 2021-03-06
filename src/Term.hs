module Term where

import IO ( Input, Output, input, output, parsecReader )

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec.Expr as Expr
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
           ( CharParser, Parser, getPosition, (<|>), (<?>), )
import Text.ParserCombinators.Parsec.Pos
           ( SourcePos, )
import Text.ParserCombinators.Parsec.Expr
           ( Assoc(AssocLeft, AssocRight, AssocNone) )
import Text.PrettyPrint.HughesPJ ( Doc, (<+>), fsep, parens, render, text )

import qualified Data.Set as S
import Control.Monad.Exception.Synchronous ( Exceptional(Success,Exception) )
import Control.Monad ( liftM2, mzero )
import Control.Functor.HT ( void )
import Data.Char (isUpper, isLower)
import Data.Ord (comparing)


data Range = Range { start :: SourcePos , end :: SourcePos }
    deriving (Eq, Ord, Show)

data Identifier =
     Identifier { range :: Range, name :: String }

instance Eq Identifier where
-- | FIXME: this is ignoring the module.
-- for a complete implementation, we'd need fully qualified names
    i == j = name i == name j

instance Ord Identifier where
    compare = comparing name

isConstructor :: Identifier -> Bool
isConstructor i =
    case name i of
        c:_ -> c == '[' || c == ':' || isUpper c
        _ -> error "isConstructor: identifier must be non-empty"

isVariable :: Identifier -> Bool
isVariable i =
    case name i of
        c:_ -> isLower c || elem c ('_':operatorSymbols)
        _ -> error "isVariable: identifier must be non-empty"


lexer :: T.TokenParser st
lexer =
   T.makeTokenParser $ L.emptyDef {
      L.commentStart = "{-",
      L.commentEnd = "-}",
      L.commentLine = "--",
      L.nestedComments = True,
      L.identStart = identifierStart,
      L.identLetter = identifierLetter,
      L.opStart = operatorStart,
      L.opLetter = operatorLetter,
      L.caseSensitive = True,
      L.reservedNames = [ "module", "where", "import", "qualified"
                        , "as", "data", "class", "instance", "case", "of"
                        , "infix", "infixl", "infixr" ],
      L.reservedOpNames = [ "=", "::", "|" ]
      }


-- FIXME: this should be read from a file (Prelude.hs).
-- but then we need a parser that correctly handles fixity information
-- on-the-fly.
-- A simplified solution could be:
-- Allow fixity definitions only between import and the first declaration.
-- With this restriction we could parse the preamble first
-- and then start with a fresh parser for the module body.
-- For now, we hard-code Prelude's fixities:
{-


infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`
-}

operators :: [[([Char], Assoc)]]
operators =
  [ [ ( ".", AssocRight ), ( "!!", AssocLeft ) ]
  , [ ( "^", AssocRight) ]
  , [ ( "*", AssocLeft), ("/", AssocLeft), ("%", AssocLeft), ("+:+", AssocRight) ]
  , [ ( "+", AssocLeft), ("-", AssocLeft), ("=:=", AssocRight) ]
  , [ ( ":", AssocRight ), ( "++", AssocRight ) ]
  , map ( \ s -> (s, AssocNone) ) [ "==", "/=", "<", "<=", ">=", ">" ]
  , [ ( "&&", AssocRight ) ]
  , [ ( "||", AssocRight ) ]
  , [ ( "$",  AssocRight ) ]
  ]

identifierStart, identifierLetter :: CharParser st Char
identifierStart = Parsec.letter <|> Parsec.char '_'

-- FIXME: check the distinction between '.' in qualified names, and as operator
identifierLetter =
    Parsec.alphaNum <|> Parsec.char '_' <|> Parsec.char '.'

identifierCore :: Parser String
identifierCore =
    liftM2 (:) identifierStart (Parsec.many identifierLetter)

identifier :: Parser String
identifier = T.identifier lexer

parenOperator :: Parser Identifier
parenOperator =
    T.parens lexer $ T.lexeme lexer $
    fmap (uncurry Identifier) $ ranged $
    liftM2 (:) operatorStart (Parsec.many operatorLetter)

infixOperator :: Parser Identifier
infixOperator =
    T.lexeme lexer $
    fmap (uncurry Identifier) $ ranged $
       Parsec.between (Parsec.char '`') (Parsec.char '`') identifierCore
       <|>
       liftM2 (:) operatorStart (Parsec.many operatorLetter)

symbol :: String -> Parser ()
symbol = void . T.symbol lexer

ranged :: CharParser st a -> CharParser st (Range, a)
ranged p = do
    from <- getPosition
    x <- p
    to <- getPosition
    return $ (Range from to, x)


instance Input Identifier where
  input =
      T.lexeme lexer $
      fmap (uncurry Identifier) $ ranged identifierCore

instance Output Identifier where
  output i = text $ name i

instance Show Identifier where show = render . output
instance Read Identifier where readsPrec = parsecReader


data Term = Node Identifier [ Term ]
          | Number Range Integer
          | StringLiteral Range String
    deriving ( Eq, Ord )

instance Show Term where show = render . output
instance Read Term where readsPrec = parsecReader


mainName :: Term
mainName = read "main"


{- |
simplifies case analysis
-}
viewNode :: Term -> Maybe (String, [Term])
viewNode (Node f xs) = Just (Term.name f, xs)
viewNode _ = Nothing

appendArguments :: Term -> [Term] -> Exceptional String Term
appendArguments g ys =
    case (g, ys) of
        (Node f xs, _) -> return $ Node f $ xs ++ ys
        (t, []) -> return t
        (t, _) ->
            Exception $
            unwords [ "cannot apply ", show t,
                      "to arguments like a function" ]

{- |
I would like to use 'T.stringLiteral'
but this skips trailing spaces
and we need the precise range of the literal.
However this implementation is very simplistic,
since T.stringChar is not exported.
-}
parseStringLiteral :: Parsec.GenParser Char st String
parseStringLiteral =
    flip (<?>) "literal string" $
--    fmap catMaybes $
    Parsec.between
        (Parsec.char '"')
        (Parsec.char '"' <?> "end of string")
        (Parsec.many (Parsec.noneOf $ '"':"\n\r\\"))
--        (Parsec.many (T.stringChar lexer))


parseAtom :: Parser Term
parseAtom =
        (T.lexeme lexer $ fmap (uncurry Number) $
         ranged (fmap read $ Parsec.many1 Parsec.digit))
    <|> fmap (uncurry StringLiteral)
             (T.lexeme lexer (ranged parseStringLiteral))
--    <|> fmap (uncurry StringLiteral) (ranged (T.stringLiteral lexer))
    <|> T.parens lexer input
    <|> bracketedList
    <|> fmap (flip Node []) input

parse :: Parser Term
parse = do
    t <- liftM2 appendArguments parseAtom $ Parsec.many parseAtom
    case t of
        Success t' -> return t'
        Exception e -> fail e

instance Input Term where
  input = Expr.buildExpressionParser table parse

operatorStart, operatorLetter :: CharParser st Char
operatorStart  = Parsec.oneOf operatorSymbols
operatorLetter = Parsec.oneOf operatorSymbols

operatorSymbols :: [Char]
operatorSymbols = ":!#$%&*+./<=>?@\\^|-~"

table :: Expr.OperatorTable Char st Term
table = map ( map binary ) operators

binary :: (String, Assoc) -> Expr.Operator Char st Term
binary (s, assoc) = flip Expr.Infix assoc $ do
    rng <- Parsec.try $ T.lexeme lexer $ do
        (rng,_) <- ranged $ Parsec.string s
        Parsec.notFollowedBy operatorLetter <?> ("end of " ++ show s)
        return rng
    return $ \ l r -> Node ( Identifier { name = s, range = rng } ) [ l, r ]


bracketedList :: Parser Term
bracketedList = do
    (r,_) <- ranged $ symbol "["
    insideBracketedList r

insideBracketedList :: Range -> Parser Term
insideBracketedList rng =
        do (r,_) <- ranged $ symbol "]"
           return $ Node ( Identifier { name = "[]", range = r } ) []
    <|> do x <- input
           q <- getPosition
           xs <-   do symbol "]" ; r <- getPosition
                      return $ Node ( Identifier { name = "[]", range = Range q r } ) []
               <|> do symbol "," ; r <- getPosition
                      insideBracketedList $ Range q r
           return $ Node ( Identifier { name = ":", range = rng } ) [ x, xs ]

instance Output Term where
  output t = case t of
     Number _ n -> text $ show n
     StringLiteral _ s -> text $ show s
     Node f args -> output f <+> fsep ( map protected args )

protected :: Term -> Doc
protected t = case t of
  Node _f (_:_) -> parens $ output t
  _ -> output t


termRange :: Term -> Range
termRange (Node i _) = range i
termRange (Number rng _) = rng
termRange (StringLiteral rng _) = rng

{- |
compute the number of nodes in the same depth
-}
breadths :: Term -> [ Int ]
breadths t = 1 : case t of
    Node _f xs -> foldl addList [] $ map breadths xs
    _ -> []

addList :: [ Int ] -> [ Int ] -> [ Int ]
addList (x:xs) (y:ys) = (x+y) : addList xs ys
addList [] ys = ys
addList xs [] = xs


type Position = [ Int ]

subterms :: Term -> [ (Position, Term) ]
subterms t = ( [], t ) : case t of
    Node _f xs -> do
        (k, x) <- zip [ 0.. ] xs
        (p, s) <- subterms x
        return (k : p, s)
    _ -> []

signature :: Term -> S.Set Identifier
signature t = S.fromList $ do
    (_p, Node f _xs) <- subterms t
    return f

peek :: Term -> Position -> Maybe Term
peek t [] = return t
peek (Node _f xs) (k : ks) =
    case drop k xs of
        x:_ -> peek x ks
        [] -> mzero
peek _ _  = mzero

poke :: Term -> Position -> Term -> Maybe Term
poke _t [] s = return s
poke (Node f xs) (k : ks) s =
    case splitAt k xs of
        (pre, x : post) -> do
            y <- poke x ks s
            return $ Node f $ pre ++ y : post
        (_, []) -> error "Term.poke: index too large"
poke _ (_:_) _ =
    error "Term.poke: cannot access a leaf with an index"
