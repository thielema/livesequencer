module Term where

import IO
import Common ( void )

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import Text.ParserCombinators.Parsec ( Parser )
import Text.Parsec as Parsec
import Text.PrettyPrint.HughesPJ

import qualified Data.Set as S
import Control.Monad ( mzero )
import Data.Char (isUpper, isLower)

data Identifier =  Identifier { name :: String , position :: SourcePos }

instance Eq Identifier where
-- | FIXME: this is ignoring the module.
-- for a complete implementation, we'd need fully qualified names
    i == j = name i == name j

instance Ord Identifier where
    compare i j = compare ( name i ) ( name j )

isConstructor :: Identifier -> Bool
isConstructor i = isUpper $ head $ name i

isVariable :: Identifier -> Bool
isVariable i = isLower $ head $ name i



lexer :: T.TokenParser st
lexer =
   T.makeTokenParser $ L.emptyDef {
      L.commentStart = "{-",
      L.commentEnd = "-}",
      L.commentLine = "--",
      L.nestedComments = True,
      L.identStart = letter <|> Parsec.char '_',
      L.identLetter = alphaNum <|> Parsec.char '_',
      L.caseSensitive = True
   }


identifier :: Parser String
identifier = T.identifier lexer

symbol :: String -> Parser ()
symbol = void . T.symbol lexer


instance Input Identifier where
  input = do
      p <- getPosition
      x <- identifier
      return $ Identifier { name = x , position = p }

instance Output Identifier where
  output i = text $ name i

data Term = Node Identifier [ Term ]
          | Number Integer  -- ^ FIXME: Number is missing source information
    deriving ( Eq, Ord )

instance Show Term where show = render . output
instance Read Term where readsPrec = parsec_reader


instance Input Term where
  input = let p atomic =
                     fmap Number (T.integer lexer)
                 <|> T.parens lexer input
                 <|> bracketed_list
                 <|> do f <- input ; args <- if atomic then return [] else many ( p True )
                        return $ Node f args
          in  p False

bracketed_list :: Parser Term
bracketed_list = do
    start <- getPosition ; symbol "["
    inside_bracketed_list start

inside_bracketed_list :: SourcePos -> Parser Term
inside_bracketed_list p =
        do q <- getPosition ; symbol "]"
           return $ Node ( Identifier { name = "Nil", position = q } ) []
    <|> do x <- input
           q <- getPosition
           xs <-   do symbol "]"
                      return $ Node ( Identifier { name = "Nil", position = q } ) []
               <|> do symbol ","
                      inside_bracketed_list q
           return $ Node ( Identifier { name = "Cons", position = p } ) [ x, xs ]

instance Output Term where
  output t = case t of
     Number n -> text $ show n
     Node f args -> output f <+> fsep ( map protected args )

protected :: Term -> Doc
protected t = case t of
  Node f args | not ( null args ) -> parens $ output t
  _ -> output t


type Position = [ Int ]

subterms :: Term -> [ (Position, Term) ]
subterms t = ( [], t ) : case t of
    Node f xs -> do 
        (k, x) <- zip [ 0.. ] xs
        (p, s) <- subterms x            
        return (k : p, s)
    _ -> []    
    
signature :: Term -> S.Set Identifier
signature t = S.fromList $ do 
    (p, Node f xs) <- subterms t
    return f

peek :: Term -> Position -> Maybe Term
peek t [] = return t
peek (Node f xs) (k : ks) | k < length xs =
    peek (xs !! k) ks
peek _ _  = mzero    
  
poke :: Term -> Position -> Term -> Maybe Term    
poke t [] s = return s
poke (Node f xs) (k : ks) s | k < length xs = do
    let (pre, x : post) = splitAt k xs
    y <- poke x ks s
    return $ Node f $ pre ++ y : post
