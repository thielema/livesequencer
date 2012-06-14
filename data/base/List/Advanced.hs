module List.Advanced where
{-
This module contains implementations of list functions
using higher-order functions like foldr.
This allows concise definitions
but looks ugly in the reduction window
and needs usually more reductions.
-}

import ListLive ( cons, append )
import Bool


(++) :: [a] -> [a] -> [a] ;
xs ++ ys = foldr cons ys xs ;

concat :: [[a]] -> [a] ;
concat = foldr append [];


take :: Int -> [a] -> [a] ;
take n xs = foldr takeElem (const []) xs n ;

takeElem :: a -> (Int -> [a]) -> Int -> [a] ;
takeElem _ _go 0 = [] ;
takeElem x go m = x : go (m-1) ;


filter :: (a -> Bool) -> [a] -> [a] ;
filter p =
   foldr (filterElem p) [] ;

filterElem :: (a -> Bool) -> a -> [a] -> [a] ;
filterElem p x xs = ifThenElse (p x) (x:xs) xs ;

takeWhile :: (a -> Bool) -> [a] -> [a] ;
takeWhile p =
   foldr (takeWhileElem p) [] ;

takeWhileElem :: (a -> Bool) -> a -> [a] -> [a] ;
takeWhileElem p x xs = ifThenElse (p x) (x:xs) [] ;
