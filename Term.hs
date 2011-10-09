module Term where

import IO

import Text.Parsec
import Text.PrettyPrint.HughesPJ

import qualified Data.Set as S
import Control.Monad ( mzero )
import Data.Char (isUpper, isLower)
                     
newtype Identifier = Identifier String 
                   deriving ( Eq, Ord )
                            
isConstructor :: Identifier -> Bool
isConstructor (Identifier s) = isUpper $ head s

isVariable :: Identifier -> Bool
isVariable (Identifier s) = isLower $ head s

instance Input Identifier where
  input = do x <- letter ; xs <- many alphaNum ; spaces ; return $ Identifier $ x : xs

instance Output Identifier where
  output ( Identifier s ) = text s

data Term = Node Identifier [ Term ]
          | Number Integer
    deriving ( Eq, Ord )
             
instance Show Term where show = render . output
instance Read Term where readsPrec = parsec_reader
    

instance Input Term where 
  input = let p atomic = do ds <- many1 digit ; spaces ; return $ Number $ read ds
                 <|> do string "(" ; spaces ; x <- p False ; string ")" ; spaces ; return x
                 <|> do f <- input ; args <- if atomic then return [] else many ( p True )
                        return $ Node f args
          in  p False


instance Output Term where
  output t = case t of
     Number n -> text $ show n
     Node f args -> output f <+> fsep ( map protected args )
     
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
