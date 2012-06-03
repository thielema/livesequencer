module Distributed.Utility where

multiIndex :: [a] -> [Int] -> [a] ;
multiIndex xs ns = map ( index xs ) ns ;

index :: [a] -> Int -> a ;
index xs n = xs !! n ;
