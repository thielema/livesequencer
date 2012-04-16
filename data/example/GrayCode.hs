module GrayCode where

import Midi ;
import Pitch ;
import Tuple ;
import List () ;
import Bool (ifThenElse) ;
import ListLive ;
import Prelude hiding (fst, snd) ;


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $ changeTempo timeUnit $ cycle overlapping ;

shortChords, overlapping :: [ Event Message ] ;
shortChords =
   concatMap chordFromCode $ grayCodes 4 ;

chordFromCode :: [ Bool ] -> [ Event Message ] ;
chordFromCode bits =
   (filterWith bits $ map noteOn pitches)
   ++
   Wait qn
   :
   (filterWith bits $ map noteOff pitches) ;

filterWith :: [ Bool ] -> [ a ] -> [ a ] ;
filterWith bits =
   map snd . filter fst . zipWith Pair bits ;


overlapping =
   concatMap (cons (Wait qn) . flip cons []) $
   grayChanges makeNoteOnOff 4 ;

makeNoteOnOff :: Bool -> Integer -> Event Message ;
makeNoteOnOff bl n =
   ifThenElse bl noteOn noteOff $ makePitch n ;

pitches :: [ Pitch ] ;
pitches = map makePitch $ enumFrom 0 ;

makePitch :: Integer -> Pitch ;
makePitch 0 = c 4 ;
makePitch 1 = e 4 ;
makePitch 2 = g 4 ;
makePitch _ = c 5 ;


-- * Enumerate binary codes

{- |
Enumerate binary codes where only one bit changes between adjacent codes.
-}
grayCodes :: Integer -> [[Bool]] ;
grayCodes 0 = [[]] ;
grayCodes n =
   map (cons False) (grayCodes (n-1)) ++
   map (cons True) (reverse $ grayCodes (n-1)) ;


{- |
Positions that change in the gray code
bundled with the bit value after the change.
-}
grayChanges :: (Bool -> Integer -> a) -> Integer -> [a] ;
grayChanges mk n =
   grayChangesRec mk True n ++ [mk False n] ;

grayChangesRec :: (Bool -> Integer -> a) -> Bool -> Integer -> [a] ;
grayChangesRec _mk _bl 0 = [] ;
grayChangesRec mk bl n =
   grayChangesRec mk True  (n-1) ++
   mk bl (n-1) :
   grayChangesRec mk False (n-1) ;


-- * auxiliary

timeUnit :: Time ;
timeUnit = 150 ;

qn :: Integer ;
qn = 1 ;
