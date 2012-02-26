module Fibonacci where

import Midi ;
import List ;
import Pitch ;
import Function ;
import Prelude ( Integer, (*), (+), mod ) ;


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $
   concatMap (note qn . makePitch) $
   modFibonacci3 4 1 1 0 ;

makePitch :: Integer -> Pitch ;
makePitch 0 = c 4 ;
makePitch 1 = e 4 ;
makePitch 2 = g 4 ;
makePitch _ = c 5 ;


-- * Solution of linear difference equation in modulo arithmetic

modFibonacci :: Integer -> Integer -> Integer -> [Integer] ;
modFibonacci n c0 c1 = modFibonacciRec n c0 c1 0 1 ;

modFibonacciRec ::
   Integer -> Integer -> Integer ->
   Integer -> Integer -> [Integer] ;
modFibonacciRec n c0 c1 x0 x1 =
   x0 : applyStrict (modFibonacciRec n c0 c1 x1) (mod (x0*c0+x1*c1) n) ;


modFibonacci3 :: Integer -> Integer -> Integer -> Integer -> [Integer] ;
modFibonacci3 n c0 c1 c2 = modFibonacci3Rec n c0 c1 c2 0 0 1 ;

modFibonacci3Rec ::
   Integer -> Integer -> Integer -> Integer ->
   Integer -> Integer -> Integer -> [Integer] ;
modFibonacci3Rec n c0 c1 c2 x0 x1 x2 =
   x0 : applyStrict (modFibonacci3Rec n c0 c1 c2 x1 x2) (mod (x0*c0+x1*c1+x2*c2) n) ;


-- * auxiliary

qn :: Integer ;
qn = 150 ;
