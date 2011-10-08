module Program where

import IO
import Term
import Rule

import Text.Parsec
import Text.PrettyPrint.HughesPJ

data Program = Program { rules :: [ Rule ] }

instance Input Program where
  input = do rs <- many input ; return $ Program { rules = rs }
instance Output Program where 
  output p = vcat $ map output $ rules p
  
instance Show Program where show = render . output
instance Read Program where readsPrec = parsec_reader
  