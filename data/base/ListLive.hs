module ListLive (
    cons,
    append,
    splitAt,
    span,
    afterEach,
    dropWhileRev,

    sumInteger,
    productInteger,
    iterateInteger,
    iterateIntegerList,
    applyStrictList,
    applyStrictListList,
    ) where

import Tuple
import Function
import Bool
import Prelude ( (-), (+), (*), Num, Int, Integer, Integral, Bool, foldr, null, reverse )


cons :: a -> [a] -> [a] ;
cons x xs = x : xs ;

append :: [a] -> [a] -> [a] ;
append = flip ( foldr cons ) ;


{-
This is not very efficient
since the result of the recursive call to 'splitAt' cannot be shared.
-}
splitAt :: Int -> [a] -> Tuple.Pair [a] [a] ;
splitAt 0 xs = Pair [] xs ;
splitAt _ [] = Pair [] [] ;
splitAt n (x : xs) = consFirst x ( splitAt (n-1) xs ) ;

span :: (a -> Bool) -> [a] -> Tuple.Pair [a] [a] ;
span _ [] = Pair [] [] ;
span p (x : xs) =
   ifThenElse (p x) ( consFirst x ( span p xs ) ) (Pair [] (x:xs)) ;

consFirst :: a -> Tuple.Pair [a] [a] -> Tuple.Pair [a] [a] ;
consFirst x p = Pair (x : fst p) (snd p) ;


afterEach :: a -> [a] -> [a] ;
afterEach _y [] = [] ;
afterEach y (x : xs) = x : y : afterEach y xs ;


dropWhileRev :: (a -> Bool) -> [a] -> [a] ;
dropWhileRev p =
   foldr (dropWhileRevElem p) [] ;

dropWhileRevElem :: (a -> Bool) -> a -> [a] -> [a] ;
dropWhileRevElem p x xs = ifThenElse (p x && null xs) [] (x:xs) ;


-- * functions that are somehow strict

-- | constant space usage in contrast to 'sum'
sumInteger :: (Integral a) => [a] -> a ;
sumInteger = sumIntegerAux 0 ;

sumIntegerAux :: (Integral a) => a -> [a] -> a ;
sumIntegerAux 0 [] = 0 ;
sumIntegerAux s [] = s ;
sumIntegerAux s (x:xs) = sumIntegerAux (s+x) xs ;


productInteger :: (Integral a) => [a] -> a ;
productInteger = productIntegerAux 1 ;

productIntegerAux :: (Integral a) => a -> [a] -> a ;
productIntegerAux 0 [] = 0 ;
productIntegerAux p [] = p ;
productIntegerAux p (x:xs) = productIntegerAux (p*x) xs ;


-- | constant space usage in contrast to 'iterate'
iterateInteger, iterateIntegerAux ::
   (Integer -> Integer) -> Integer -> [Integer] ;
iterateInteger f = applyStrict (iterateIntegerAux f) ;
iterateIntegerAux f x = x : iterateInteger f (f x) ;

iterateIntegerList, iterateIntegerListAux ::
   ([Integer] -> [Integer]) -> [Integer] -> [[Integer]] ;
iterateIntegerList f = applyStrictList (iterateIntegerListAux f) ;
iterateIntegerListAux f x = x : iterateIntegerList f (f x) ;

{- even stricter: it always updates the accumulator, also if the updated value is not needed because the list is aborted earlier
iterateInteger, iterateIntegerAux0 ::
   (Integer -> Integer) -> Integer -> [Integer] ;
iterateIntegerAux1 ::
   (Integer -> Integer) -> Integer -> Integer -> [Integer] ;
iterateInteger f = applyStrict (iterateIntegerAux0 f) ;
iterateIntegerAux0 f x = applyStrict (iterateIntegerAux1 f x) (f x) ;
iterateIntegerAux1 f x fx = x : iterateInteger f fx ;
-}

{- too strict for DeBruijn
iterateIntegerList, iterateIntegerListAux0 ::
   ([Integer] -> [Integer]) -> [Integer] -> [[Integer]] ;
iterateIntegerListAux1 ::
   ([Integer] -> [Integer]) -> [Integer] -> [Integer] -> [[Integer]] ;
iterateIntegerList f = applyStrictList (iterateIntegerListAux0 f) ;
iterateIntegerListAux0 f x = applyStrictList (iterateIntegerListAux1 f x) (f x) ;
iterateIntegerListAux1 f x fx = x : iterateIntegerList f fx ;
-}



applyStrictList :: ([Integer] -> a) -> ([Integer] -> a) ;
applyStrictList f xs = applyStrictListAux f [] (reverse xs) ;

applyStrictListAux :: ([Integer] -> a) -> [Integer] -> ([Integer] -> a) ;
applyStrictListAux f ys [] = f ys ;
applyStrictListAux f ys (0:xs) = applyStrictListAux f (0:ys) xs ;
applyStrictListAux f ys (x:xs) = applyStrictListAux f (x:ys) xs ;


applyStrictListList :: ([[Integer]] -> a) -> ([[Integer]] -> a) ;
applyStrictListList f xs = applyStrictListListAux f [] (reverse xs) ;

applyStrictListListAux :: ([[Integer]] -> a) -> [[Integer]] -> ([[Integer]] -> a) ;
applyStrictListListAux f ys [] = f ys ;
applyStrictListListAux f ys (x:xs) =
   applyStrictList (applyStrictListListAux f . flip cons ys) x xs ;
