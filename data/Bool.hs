module Bool where

ifThenElse :: Bool -> a -> a -> a ;
ifThenElse True  y _ = y ;
ifThenElse False _ n = n ;

not :: Bool -> Bool ;
not False = True ;
not True = False ;

(&&) :: Bool -> Bool -> Bool ;
False && _ = False ;
True && x = x ;

(||) :: Bool -> Bool -> Bool ;
True || _ = True ;
False || x = x ;
