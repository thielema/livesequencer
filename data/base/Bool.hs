module Bool where

import Prelude ( Bool(True, False) )


ifThenElse :: Bool -> a -> a -> a ;
ifThenElse True  y _ = y ;
ifThenElse False _ n = n ;


not :: Bool -> Bool ;
not False = True ;
not True = False ;

-- the same as (/=) for Bool
xor :: Bool -> Bool -> Bool ;
xor False True = True ;
xor True False = True ;
xor _ _ = False ;


infixr 3 && ;

(&&) :: Bool -> Bool -> Bool ;
False && _ = False ;
True && x = x ;


infixr 2 || ;

(||) :: Bool -> Bool -> Bool ;
True || _ = True ;
False || x = x ;
