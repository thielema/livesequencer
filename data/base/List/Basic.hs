module List.Basic where
{-
This module contains implementations of list functions
using plain recursion.
The definitions become a bit longer
but the reductions look a bit nicer
and there are usually less one than in higher-order style.
-}

import Bool
import Prelude ( (-), Num, Int, Bool )


(++) :: [a] -> [a] -> [a] ;
(x:xs) ++ ys = x : (xs ++ ys) ;
[] ++ ys = ys ;

concat :: [[a]] -> [a] ;
concat (x:xs) = x ++ concat xs ;
concat [] = [] ;


take :: Int -> [a] -> [a] ;
take 0 _xs = [] ;
take n (x:xs) = x : take (n-1) xs ;
take _ [] = [] ;

filter :: (a -> Bool) -> [a] -> [a] ;
filter p (x:xs) = ifThenElse (p x) (x : filter p xs) (filter p xs) ;
filter _ [] = [] ;

takeWhile :: (a -> Bool) -> [a] -> [a] ;
takeWhile p (x:xs) = ifThenElse (p x) (x : takeWhile p xs) [] ;
takeWhile _ [] = [] ;
