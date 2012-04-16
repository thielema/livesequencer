module DeBruijn where

import Midi ;
import ListLive ;
import List ;
import Pitch ;
import Bool ;
import Integer ;
import Prelude ( Integer, fromInteger, fromIntegral, ($), (.), (+), (-), (<), mod ) ;


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $ changeTempo timeUnit $
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
   foldr (checkLyndonElement n) [] . take (fromInteger k) . cycle ;

checkLyndonElement :: Integer -> Integer -> [Integer] -> [Integer] ;
checkLyndonElement n x [] = ifThenElse (x<n-1) [x+1] [] ;
checkLyndonElement _ x xs = x:xs ;

deBruijnSequence :: Integer -> Integer -> [Integer] ;
deBruijnSequence n k =
   concat $
   filter (isZero . mod k . fromIntegral . length) $
   takeWhile (not . null) $
   iterateIntegerList (nextLyndonWord n k) [0] ;


-- Another efficient approach might be encoding the Lyndon words as integers.


-- * auxiliary

timeUnit :: Time ;
timeUnit = 150 ;

qn :: Integer ;
qn = 1 ;
