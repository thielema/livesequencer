module List where

import Midi
import Tuple
import Prelude ( (-), compare, Ordering(LT,EQ,GT) )


replicate n x = take n ( repeat x ) ;

repeat s = s : repeat s ;

cycle s = append s (cycle s) ;

append [] ys = ys ;
append (x : xs) ys = x : append xs ys ;

concat [] = [] ;
concat (x : xs) = append x (concat xs) ;

take 0 xs = [] ;
take n [] = [] ;
take n (x : xs) = x : take (n-1) xs ;

drop 0 xs = xs ;
drop n [] = [] ;
drop n (x : xs) = drop (n-1) xs ;

{-
This does not work well and fails for infinite lists,
because consFirst matches strictly on Pair.
-}
splitAt 0 xs = Pair [] xs ;
splitAt n [] = Pair [] [] ;
splitAt n (x : xs) = consFirst x ( splitAt (n-1) xs ) ;

consFirst x (Pair xs ys) = Pair (x : xs) ys ;


merge (Wait a : xs) (Wait b : ys) =
  mergehelper (compare a b) a xs b ys ;
merge (Wait a : xs) (y : ys) =
  y : merge (Wait a : xs) ys ;
merge (x : xs) (Wait b : ys) =
  x : merge xs (Wait b : ys) ;
merge (x : xs) ys = x : merge xs ys ;
merge [] ys = ys ;

mergehelper LT  a xs b ys =
  Wait a : merge xs (Wait (b - a) : ys) ;
mergehelper EQ  a xs b ys =
  Wait a : merge xs ys ;
mergehelper GT a xs b ys =
  Wait b : merge (Wait (a - b) : xs) ys ;
