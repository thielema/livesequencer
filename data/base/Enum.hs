module Enum where

import Prelude (Eq, Num, (+), (-), ) ;

succ, pred :: (Num a) => a -> a ;
succ x = x+1 ;
pred x = x-1 ;

enumFrom, enumFromLazy :: (Eq a, Num a) => a -> [a] ;
enumFrom 0 = enumFromLazy 0 ;
enumFrom n = enumFromLazy n ;

enumFromLazy n = n : enumFrom (succ n) ;
