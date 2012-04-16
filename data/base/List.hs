module List (
    map,
    zipWith,
    zipWith3,
    foldr,
    foldl,
    length,
    sum,
    add,
    scanl,
    scanr,
    reverse,
    replicate,
    repeat,
    cycle,
    iterate,
    (++),
    concat,
    concatMap,
    head,
    tail,
    null,
    (!!),
    take,
    drop,
    filter,
    takeWhile,
    inits,
    tails,
    ) where

import ListLive
import Function
import Bool
import Prelude ( (-), (+), Num, Int, Bool(False,True), error )


map :: (a -> b) -> [a] -> [b] ;
map _ [] = [] ;
map f (x : xs) = f x : map f xs ;

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] ;
zipWith f (x : xs) (y : ys) =
    f x y : zipWith f xs ys ;
zipWith _f _xs _ys = [] ;

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d] ;
zipWith3 f (x : xs) (y : ys) (z : zs) =
    f x y z : zipWith3 f xs ys zs ;
zipWith3 _f _xs _ys _zs = [] ;

foldr :: (b -> a -> a) -> a -> [b] -> a ;
foldr _ a [] = a ;
foldr f a (x : xs) = f x ( foldr f a xs ) ;

foldl :: (b -> a -> b) -> b -> [a] -> b ;
foldl _ a [] = a ;
foldl f a (x : xs) = foldl f (f a x) xs ;

length :: [a] -> Int ;
length = sumInteger . map (const 1) ;

sum :: (Num a) => [a] -> a ;
sum = foldl add 0 ;

add :: (Num a) => a -> a -> a ;
add x y = x + y ;


scanl :: (a -> b -> a) -> a -> [b] -> [a] ;
scanl _ a [] = [a] ;
scanl f a (x : xs) = a : scanl f (f a x) xs ;

scanr :: (b -> a -> a) -> a -> [b] -> [a] ;
scanr _ a [] = [a] ;
scanr f a (x : xs) = scanrAux f x (scanr f a xs) ;

scanrAux :: (b -> a -> a) -> b -> [a] -> [a] ;
scanrAux f x ys = f x (head ys) : ys ;


reverse :: [a] -> [a] ;
reverse = foldl (flip cons) [] ;

replicate :: Int -> a -> [a] ;
replicate n x = take n ( repeat x ) ;

repeat :: a -> [a] ;
repeat s = s : repeat s ;

cycle :: [a] -> [a] ;
cycle s = s ++ cycle s ;

iterate :: (a -> a) -> a -> [a] ;
iterate f x = x : iterate f (f x) ;


(++) :: [a] -> [a] -> [a] ;
xs ++ ys = foldr cons ys xs ;

concat :: [[a]] -> [a] ;
concat = foldr append [];

concatMap :: (a -> [b]) -> [a] -> [b] ;
concatMap f = concat . map f ;

head :: [a] -> a ;
head (x:_) = x ;
head [] = error "head: empty list" ;

tail :: [a] -> [a] ;
tail (_:xs) = xs ;
tail [] = error "tail: empty list" ;

null :: [a] -> Bool ;
null [] = True ;
null _ = False ;

(!!) :: [a] -> Int -> a ;
(x:_)  !! 0 = x ;
(_:xs) !! n = xs !! (n-1) ;
[] !! _ = error "!!: index too large" ;

take :: Int -> [a] -> [a] ;
take n xs = foldr takeElem (const []) xs n ;

takeElem :: a -> (Int -> [a]) -> Int -> [a] ;
takeElem _ _go 0 = [] ;
takeElem x go m = x : go (m-1) ;

drop :: Int -> [b] -> [b] ;
drop 0 xs = xs ;
drop _ [] = [] ;
drop n (_ : xs) = drop (n-1) xs ;


filter :: (a -> Bool) -> [a] -> [a] ;
filter p =
   foldr (filterElem p) [] ;

filterElem :: (a -> Bool) -> a -> [a] -> [a] ;
filterElem p x xs = ifThenElse (p x) (x:xs) xs ;

takeWhile :: (a -> Bool) -> [a] -> [a] ;
takeWhile p =
   foldr (takeWhileElem p) [] ;

takeWhileElem :: (a -> Bool) -> a -> [a] -> [a] ;
takeWhileElem p x xs = ifThenElse (p x) (x:xs) [] ;


inits :: [a] -> [[a]] ;
inits xs = [] : initsAux xs ;

initsAux :: [a] -> [[a]] ;
initsAux [] = [] ;
initsAux (x:xs) = map (cons x) (inits xs) ;

tails :: [a] -> [[a]] ;
tails xs = xs : tailsAux xs ;

tailsAux :: [a] -> [[a]] ;
tailsAux [] = [] ;
tailsAux (_:xs) = tails xs ;
