module Prelude where

import Bool
import Integer
import Enum


data Ordering = LT | EQ | GT ;

signumFromOrdering LT = 0-1 ;
signumFromOrdering EQ = 0 ;
signumFromOrdering GT = 1 ;

signum x = signumFromOrdering (compare x 0) ;

abs x = signum x * x ;

(==) :: Integer -> Integer -> Bool ;
x == y  =  isZero (x-y) ;

min x y = ifThenElse (x<y) x y ;

negate x = 0 - x ;

-- fromInteger :: Integer -> Int ;
fromInteger x = x ;

-- fromIntegral :: Int -> Integer ;
fromIntegral x = x ;
