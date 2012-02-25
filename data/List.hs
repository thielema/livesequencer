module List where

import Midi
import Tuple
import Function
import Bool
import Prelude ( (-), (+), (<), negate, Num, Integer, Bool(False,True), error )


map :: (a -> b) -> [a] -> [b] ;
map _ [] = [] ;
map f (x : xs) = f x : map f xs ;

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] ;
zipWith f (x : xs) (y : ys) =
    f x y : zipWith f xs ys ;
zipWith _f _xs _ys = [] ;

foldr :: (b -> a -> a) -> a -> [b] -> a ;
foldr _ a [] = a ;
foldr f a (x : xs) = f x ( foldr f a xs ) ;

foldl :: (b -> a -> b) -> b -> [a] -> b ;
foldl _ a [] = a ;
foldl f a (x : xs) = foldl f (f a x) xs ;

length :: [a] -> Integer ;
length = sumInteger . map (const 1) ;

sum :: (Num a) => [a] -> a ;
sum = foldl add 0 ;

add :: (Num a) => a -> a -> a ;
add x y = x + y ;

-- | constant space usage in contrast to 'sum'
sumInteger :: [Integer] -> Integer ;
sumInteger = sumIntegerAux 0 ;

sumIntegerAux :: Integer -> [Integer] -> Integer ;
sumIntegerAux 0 [] = 0 ;
sumIntegerAux s [] = s ;
sumIntegerAux s (x:xs) = sumIntegerAux (s+x) xs ;


reverse :: [a] -> [a] ;
reverse = foldl (flip cons) [] ;

replicate :: Integer -> a -> [a] ;
replicate n x = take n ( repeat x ) ;

repeat :: a -> [a] ;
repeat s = s : repeat s ;

cycle :: [a] -> [a] ;
cycle s = s ++ cycle s ;

iterate :: (a -> a) -> a -> [a] ;
iterate f x = x : iterate f (f x) ;

-- | constant space usage in contrast to 'iterate'
iterateInteger :: (Integer -> Integer) -> Integer -> [Integer] ;
iterateInteger f 0 = 0 : iterateInteger f (f 0) ;
iterateInteger f x = x : iterateInteger f (f x) ;

append :: [a] -> [a] -> [a] ;
append = flip ( foldr cons ) ;

(++) :: [a] -> [a] -> [a] ;
xs ++ ys = foldr cons ys xs ;

cons :: a -> [a] -> [a] ;
cons x xs = x : xs ;

concat :: [[a]] -> [a] ;
concat = foldr append [];

(!!) :: [a] -> Integer -> a ;
(x:_)  !! 0 = x ;
(_:xs) !! n = xs !! (n-1) ;
[] !! _ = error "!!: index too large" ;

take :: Integer -> [a] -> [a] ;
take n xs = foldr takeElem (const []) xs n ;

takeElem :: a -> (Integer -> [a]) -> Integer -> [a] ;
takeElem _ _go 0 = [] ;
takeElem x go m = x : go (m-1) ;

drop :: Integer -> [b] -> [b] ;
drop 0 xs = xs ;
drop _ [] = [] ;
drop n (_ : xs) = drop (n-1) xs ;

{-
This does not work well and fails for infinite lists,
because consFirst matches strictly on Pair.
-}
splitAt :: Integer -> [a] -> Tuple.Pair [a] [a] ;
splitAt 0 xs = Pair [] xs ;
splitAt _ [] = Pair [] [] ;
splitAt n (x : xs) = consFirst x ( splitAt (n-1) xs ) ;

consFirst :: a -> Tuple.Pair [a] [a] -> Tuple.Pair [a] [a] ;
consFirst x p = Pair (x : fst p) (snd p) ;


afterEach :: a -> [a] -> [a] ;
afterEach _y [] = [] ;
afterEach y (x : xs) = x : y : afterEach y xs ;


infixr 7 +:+ ;  {- like multiplication -}
infixr 6 =:= ;  {- like addition -}

(+:+) :: [Midi.Event a] -> [Midi.Event a] -> [Midi.Event a] ;
xs +:+ ys  =  xs ++ ys ;

merge, (=:=) :: [Midi.Event a] -> [Midi.Event a] -> [Midi.Event a] ;
xs =:= ys  =  merge xs ys ;

merge (Wait a : xs) (Wait b : ys) =
  mergeWait (a<b) (a-b) a xs b ys ;
merge (Wait a : xs) (y : ys) =
  y : merge (Wait a : xs) ys ;
merge (x : xs) ys = x : merge xs ys ;
merge [] ys = ys ;

{-
This looks a bit cumbersome,
but it is necessary for avoiding stacks of unevaluated subtractions.
We use or abuse the way of how the interpreter performs pattern matching.
By matching against 0 we force the evaluation of the difference d.
The evaluated difference is hold throughout the matching of all patterns.
It is important that the match against 0 is really performed
and is not shadowed by a failing preceding match, say, against the result of (a<b).
-}
mergeWait ::
  Bool -> Integer ->
  Integer -> [Midi.Event a] ->
  Integer -> [Midi.Event a] ->
  [Midi.Event a] ;
mergeWait _eq 0 a xs _b ys =
  Wait a : merge xs ys ;
mergeWait True d a xs _b ys =
  Wait a : merge xs (Wait (negate d) : ys) ;
mergeWait False d _a xs b ys =
  Wait b : merge (Wait d : xs) ys ;

mergeMany :: [[Midi.Event a]] -> [Midi.Event a] ;
mergeMany = foldl merge [] ;
