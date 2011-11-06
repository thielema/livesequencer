module Prelude where

import Bool


data Ordering = LT | EQ | GT ;

signumFromOrdering LT = 0-1 ;
signumFromOrdering EQ = 0 ;
signumFromOrdering GT = 1 ;

signum x = signumFromOrdering (compare x 0) ;

abs x = signum x * x ;

min x y = ifThenElse (x<y) x y ;

negate x = 0 - x ;
