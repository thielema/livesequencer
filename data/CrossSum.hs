module CrossSum where

import Midi ;
import List ;
import Pitch ;
import Function ;
import Prelude ( Integer, fromInteger, (+), mod, div, succ ) ;


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $
   concatMap (note qn . makePitch) $
   modCrossSums 4 ;

makePitch :: Integer -> Pitch ;
makePitch 0 = c 4 ;
makePitch 1 = e 4 ;
makePitch 2 = g 4 ;
makePitch _ = c 5 ;


-- * explicit base conversion

modCrossSums :: Integer -> [Integer] ;
modCrossSums n =
   map (foldl (modAdd n) 0 . toBase n) $
   iterateInteger succ 0 ;

toBase :: Integer -> Integer -> [Integer] ;
toBase _n 0 = [] ;
toBase n x = mod x n : toBase n (div x n) ;


-- * recursion on lists

modCrossSumsList :: Integer -> [Integer] ;
modCrossSumsList n =
   0 : extList n [0] ;

extList :: Integer -> [Integer] -> [Integer] ;
extList n y =
   applyStrictListList (extList2 n) $ map (incList n y) $
   take (fromInteger n) $ iterateInteger succ 0 ;

extList2 :: Integer -> [[Integer]] -> [Integer] ;
extList2 n z =
   concat (tail z) ++ extList n (concat z) ;

incList :: Integer -> [Integer] -> Integer -> [Integer] ;
incList m ys x = map (modAdd m x) ys ;


-- * auxiliary

modAdd :: Integer -> Integer -> Integer -> Integer ;
modAdd m x y = mod (x+y) m ;


qn :: Integer ;
qn = 150 ;
