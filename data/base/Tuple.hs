module Tuple where

data Pair a b = Pair a b
   deriving (Show) ;

fst :: Pair a b -> a ;
fst ( Pair a _ ) = a ;

snd :: Pair a b -> b ;
snd ( Pair _ b ) = b ;
