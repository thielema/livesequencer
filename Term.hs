module Term where

import IO

import Text.Parsec
import Text.PrettyPrint.HughesPJ


                     
newtype Identifier = Identifier String 
                   deriving ( Eq, Ord )
                            
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
