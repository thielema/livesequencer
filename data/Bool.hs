module Bool where

ifThenElse :: Bool -> a -> a -> a ;
ifThenElse True  y _ = y ;
ifThenElse False _ n = n ;
