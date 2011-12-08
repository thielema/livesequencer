module Speisekarte where

import Chords
import Pitch
import Midi
import List
import Prelude ( (*), (+), (-) )


main = sopranTrack ;

sopranTrack =
   melodyChannel (
      bell ++
      controller attackCC 30 ++
      controller releaseCC 70 ++
      controller brightnessCC 0 ++
      sopranMelody
   ) ;

sopranMelody =
   rest dqn ++
   note qn (d 4) ++ note en (a 3) ++
   note en (d 4) ++ note en (d 4) ++
   note en (d 4) ++
   note qn (d 4) ++ note en (a 3) ++
   note en (d 4) ++ note en (d 4) ++
   note en (d 4) ++
   note qn (e 4) ++ note en (a 3) ++
   note en (e 4) ++ note en (e 4) ++
   note en (e 4) ++
   note qn (e 4) ++ note en (a  3) ++
   note en (e 4) ++ note en (cs 4) ++
   note en (a 3) ++
   note qn (fs 4) ++ note en (fs 4) ++
   note en (e  4) ++ note en (d  4) ++
   note en (cs 4) ++
   note qn (b  3) ++ rest en ++
   note en (g  4) ++ note en (e  4) ++
   note en (b  3) ++
   note en (cs 4) ++ note en (a  3) ++
   note en (a  3) ++ note en (a  3) ++
   note en (b  3) ++ note en (cs 4) ++
   note qn (d  4) ++ rest en ++
   sopranMelody ;


-- * concatenation

double x = concat [ x, x ] ;

quad x = concat [ x, x, x, x ] ;

quadAlt x y = concat [ x, x, x, y] ;


-- * durations

en = 230 ;
qn = 2 * en ; dqn = 3 * en ;
hn = 2 * qn ; dhn = 3 * qn ;
wn = 2 * hn ; dwn = 3 * hn ;
wn2 = 2 * wn ;


-- * MIDI program

ping = program 0 ;
slap = program 2 ;
bell = program 3 ;
pad  = program 4 ;
bass = program 9 ;


-- * MIDI channels

melodyChannel  = channel 0 ;
patternChannel = channel 1 ;
padChannel     = channel 2 ;
bassChannel    = channel 3 ;


-- * MIDI controllers

volumeCC = 7 ;
brightnessCC = 70 ;
attackCC = 73 ;
decayCC = 73 ;
releaseCC = 72 ;
