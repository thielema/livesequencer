module Distributed.Utility where

multiIndex xs ns = map ( index xs ) ns ;

index xs n = xs !! n ;
