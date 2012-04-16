module Enum where

succ x = x+1 ;
pred x = x-1 ;

enumFrom 0 = enumFromLazy 0 ;
enumFrom n = enumFromLazy n ;

enumFromLazy n = n : enumFrom (succ n) ;
