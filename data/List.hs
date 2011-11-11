module List where

import Midi
import Tuple
import Function
import Prelude ( (-), (+), (<), negate, Bool(False,True) )


map f [] = [] ;
map f (x : xs) = f x : map f xs ;

foldr f a [] = a ;
foldr f a (x : xs) = f x ( foldr f a xs ) ;

foldl f a [] = a ;
foldl f a (x : xs) = foldl f (f a x) xs ;

length = compose sum ( map (const 1) ) ;

-- think about a version with constant space usage
sum = foldl add 0 ;

add x y = x + y ;


replicate n x = take n ( repeat x ) ;

repeat s = s : repeat s ;

cycle s = append s (cycle s) ;

append = flip (foldr cons) ;

cons x xs = x : xs ;

concat = foldr append [];

take n xs = foldr takeElem (const []) xs n ;

takeElem x go 0 = [] ;
takeElem x go m = x : go (m-1) ;

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
  mergeWait (a<b) (a-b) a xs b ys ;
merge (Wait a : xs) (y : ys) =
  y : merge (Wait a : xs) ys ;
merge (x : xs) ys = x : merge xs ys ;
merge [] ys = ys ;

{-
This looks a bit cumbersome,
but it is necessary for avoiding stacks of unevaluated subtractions.
We use or abuse the way of how the interpreter performs pattern matching.
By matching against 0 we force the evaluation of the difference d.
The evaluated difference is hold throughout the matching of all patterns.
It is important that the match against 0 is really performed
and is not shadowed by a failing preceding match, say, against the result of (a<b).
-}
mergeWait eq 0 a xs b ys =
  Wait a : merge xs ys ;
mergeWait True d a xs b ys =
  Wait a : merge xs (Wait (negate d) : ys) ;
mergeWait False d a xs b ys =
  Wait b : merge (Wait d : xs) ys ;

mergeMany [] = [] ;
mergeMany (x : xs) = merge x (mergeMany xs) ;
