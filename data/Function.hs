module Function where

(f . g) x = f (g x) ;

f $ x = f x ;

flip f x y = f y x ;

id x = x ;

nest 0 f x = x ;
nest n f x = nest (n-1) f (f x) ;

const a b = a ;

fix f = f (fix f) ;
