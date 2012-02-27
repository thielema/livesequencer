module Speisekarte where

import Pitch
import Midi
import List
import Instrument
import Prelude ( (*), ($), negate )

main, loop, choir :: [Event (Channel Message)] ;
main = loop ;

loop = choir ++ loop ;

choir =
   mergeMany $
{-
      sopranoTrack :
      contraltoITrack :
      contraltoIITrack :
      tenorTrack :
      bassITrack :
-}
      bassIITrack :
   [] ;


sopranoTrack, contraltoITrack, contraltoIITrack, 
   tenorTrack, bassITrack, bassIITrack
      :: [Event (Channel Message)] ;

sopranoTrack =
   sopranoChannel (
      flutePgm ++
      transpose 12 sopranoMelody
   ) ;

contraltoITrack =
   contraltoChannel (
      pianoPgm ++
      contraltoIMelody
   ) ;

contraltoIITrack =
   contraltoChannel (
      pianoPgm ++
      contraltoIIMelody
   ) ;

tenorTrack =
   tenorChannel (
      violinPgm ++
      controller volumeCC 64 ++
      tenorMelody
   ) ;

bassITrack =
   bassChannel (
      pianoPgm ++
      bassIMelody
   ) ;

bassIITrack =
   bassChannel (
      pianoPgm ++
      transpose (negate 12) bassIIMelody
   ) ;


sopranoLoop,
   sopranoMelody, contraltoIMelody, contraltoIIMelody,
   tenorMelody, bassIMelody, bassIIMelody
      :: [Event Message] ;

sopranoLoop = sopranoMelody ++ sopranoLoop ;

sopranoMelody =
   rest dqn ++
   note qn (d 5) ++ note en (a 4) ++
   note en (d 5) ++ note en (d 5) ++
   note en (d 5) ++
   note qn (d 5) ++ note en (a 4) ++
   note en (d 5) ++ note en (d 5) ++
   note en (d 5) ++
   note qn (e 5) ++ note en (a 4) ++
   note en (e 5) ++ note en (e 5) ++
   note en (e 5) ++
   note qn (e 5) ++ note en (a 4) ++
   note en (e 5) ++ note en (cis 5) ++
   note en (a 4) ++
   note qn (fis 5) ++ note en (fis 5) ++
   note en (e 5) ++ note en (d 5) ++
   note en (cis 5) ++
   note qn (b 4) ++ rest en ++
   note en (g 5) ++ note en (e 5) ++
   note en (b 4) ++
   note en (cis 5) ++ note en (a 4) ++
   note en (a 4) ++ note en (a 4) ++
   note en (b 4) ++ note en (cis 5) ++
   note qn (d 5) ++ rest en ++
   [] ;

contraltoIMelody =
   rest en ++ note qn (a 4) ++
   note dqn (fis 4) ++ note en (a 4) ++ note qn (a 4) ++
   note hn (a 4) ++ note qn (a 4) ++
   note dqn (g 4) ++ note en (a 4) ++ note qn (a 4) ++
   note dqn (a 4) ++ note en (b 4) ++ note en (a 4) ++ note en (g 4) ++
   note dqn (fis 4) ++ note en (a 4) ++ note qn (a 4) ++
   note dqn (g 4) ++ note en (b 4) ++ note qn (b 4) ++
   note qn (a 4) ++  note qn (a 4) ++ note qn (g 4) ++
   note qn (fis 4) ++ rest en ++
   [] ;

contraltoIIMelody =
   note en (d 4) ++ note en (cis 4) ++ note en (b 3) ++
   note qn (a 3) ++ note qn (fis 4) ++ note qn (fis 4) ++
   note dqn (fis 4) ++ note en (fis 4) ++ note en (e 4) ++ note en (d 4) ++
   note qn (cis 4) ++ note qn (e 4) ++ note qn (e 4) ++
   note dqn (e 4) ++ note en (g 4) ++ note en (fis 4) ++ note en (e 4) ++
   note qn (d 4) ++ note qn (fis 4) ++ note qn (b 3) ++
   note qn (e 4) ++ note qn (g 4) ++ note qn (g 4) ++
   note qn (fis 4) ++ note qn (fis 4) ++ note qn (e 4) ++
   note qn (d 4) ++ rest en ++
   [] ;

tenorMelody =
   rest en ++ rest qn ++
   note en (a 3) ++ note en (b 3) ++ note hn (a 3) ++
   note qn (a 3) ++ note qn (d 4) ++ note qn (fis 4) ++
   note en (e 4) ++ note en (fis 4) ++ note en (e 4) ++ note en (fis 4) ++ note en (e 4) ++ note en (d 4) ++
   note hn (cis 4) ++ rest qn ++
   note en (d 4) ++ note en (b 3) ++ note hn (b 3) ++
   note qn (b 3) ++ note qn (e 4) ++ note en (b 3) ++ note en (e 4) ++
   note en (cis 4) ++ note en (d 4) ++ note en (cis 4) ++ note en (d 4) ++ note en (cis 4) ++ note en (b 3) ++
   note qn (a 3) ++ rest en ++
   [] ;

bassIMelody =
   rest en ++ rest qn ++
   rest qn ++ note en (d 3) ++ note en (fis 3) ++ note qn (fis 3) ++
   rest qn ++ note en (fis 3) ++ note en (a 3) ++ note qn (a 3) ++
   rest qn ++ note en (e 3) ++ note en (g 3) ++ note qn (g 3) ++
   rest qn ++ note en (g 3) ++ note en (cis 4) ++ note qn (cis 4) ++
   rest qn ++ note en (d 4) ++ note en (fis 3) ++ note qn (fis 3) ++
--   rest qn ++ note en (b 3) ++ note en (g 3) ++ note qn (g 3) ++
   rest en ++ note en (fis 3) ++ note en (b 3) ++ note en (g 3) ++ note qn (g 3) ++
--   rest qn ++ note en (a 3) ++ note en (e 3) ++ note en (e 3) ++ note en (e 3) ++ note qn (fis 3) ++ rest en ++
   rest en ++ note en (g 3) ++ note en (a 3) ++ note en (e 3) ++ note en (e 3) ++ note en (e 3) ++ note qn (fis 3) ++ rest en ++
   [] ;

bassIIMelody =
   rest en ++ note qn (a 2) ++
   note qn (d 3) ++ rest qn ++ note qn (d 3) ++
   note qn (d 3) ++ rest qn ++ note en (cis 3) ++ note en (b 2) ++
   note qn (a 2) ++ rest qn ++ note qn (a 2) ++
   note qn (a 2) ++ rest qn ++ note en (g 2) ++ note en (a 2) ++
   note qn (b 2) ++ rest qn ++ note en (cis 3) ++ note en (d 3) ++
   note qn (e 3) ++ rest en ++ note en (e 3) ++ note en (fis 3) ++ note en (g 3) ++
   note qn (a 3) ++ rest qn ++ note qn (a 2) ++ note qn (d 3) ++ rest en ++
   [] ;


-- * durations

en, qn, dqn, hn, dhn, wn, dwn, wn2 :: Time ;

en = 230 ;
qn = 2 * en ; dqn = 3 * en ;
hn = 2 * qn ; dhn = 3 * qn ;
wn = 2 * hn ; dwn = 3 * hn ;
wn2 = 2 * wn ;


-- * MIDI program

pianoPgm, violinPgm, flutePgm :: [Event Message] ;
pianoPgm  = program acousticGrandPiano ;
violinPgm = program stringEnsemble1 ;
flutePgm  = program flute ;


-- * MIDI channels

sopranoChannel, contraltoChannel, tenorChannel, bassChannel ::
   [Event a] -> [Event (Channel a)] ;
sopranoChannel   = channel 0 ;
contraltoChannel = channel 1 ;
tenorChannel     = channel 2 ;
bassChannel      = channel 3 ;


-- * MIDI controllers

volumeCC :: Controller ;
volumeCC = 7 ;
