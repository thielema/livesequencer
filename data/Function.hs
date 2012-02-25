module Function where


infixr 9 . ;

(.) :: (b -> c) -> (a -> b) -> a -> c ;
(f . g) x = f (g x) ;

infixr 0 $ ;

($) :: (a -> b) -> a -> b ;
f $ x = f x ;

-- $!
applyStrict :: (Integer -> a) -> (Integer -> a) ;
applyStrict f 0 = f 0 ;
applyStrict f x = f x ;

applyStrictList :: ([Integer] -> a) -> ([Integer] -> a) ;
applyStrictList f xs = applyStrictListAux f [] (reverse xs) ;

applyStrictListAux :: ([Integer] -> a) -> [Integer] -> ([Integer] -> a) ;
applyStrictListAux f ys [] = f ys ;
applyStrictListAux f ys (0:xs) = applyStrictListAux f (0:ys) xs ;
applyStrictListAux f ys (x:xs) = applyStrictListAux f (x:ys) xs ;

flip :: (b -> a -> c) -> a -> b -> c ;
flip f x y = f y x ;

id :: a -> a ;
id x = x ;

nest :: Int -> (a -> a) -> a -> a ;
nest 0 _ x = x ;
nest n f x = nest (n-1) f (f x) ;

const :: a -> b -> a ;
const a _ = a ;

fix :: (a -> a) -> a ;
fix f = f (fix f) ;
