module DeBruijn where

import Midi ;
import List ;
import Pitch ;
import Bool ;
import Integer ;
import Prelude ( Integer, ($), (.), (+), (-), (<), mod ) ;


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $
   concatMap (note qn . makePitch) $
   cycle $ deBruijnSequence 4 2 ;

makePitch :: Integer -> Pitch ;
makePitch 0 = c 4 ;
makePitch 1 = e 4 ;
makePitch 2 = g 4 ;
makePitch _ = c 5 ;


-- * De Bruijn sequence generation based on lists

nextLyndonWord :: Integer -> Integer -> [Integer] -> [Integer] ;
nextLyndonWord n k =
   foldr (checkLyndonElement n) [] . take k . cycle ;

checkLyndonElement :: Integer -> Integer -> [Integer] -> [Integer] ;
checkLyndonElement n x [] = ifThenElse (x<n-1) [x+1] [] ;
checkLyndonElement _ x xs = x:xs ;

deBruijnSequence :: Integer -> Integer -> [Integer] ;
deBruijnSequence n k =
   concat $
   filter (isZero . mod k . length) $
   takeWhile (not . null) $
   iterateIntegerList (nextLyndonWord n k) [0] ;

-- This is extremely inefficient in the live-sequencer.
-- We may achieve more efficienty by encoding every lyndon word in an integer.


-- * auxiliary

qn :: Integer ;
qn = 150 ;
