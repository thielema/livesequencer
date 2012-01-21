module GloriousKingdom where

import Chords
import Pitch
import Midi
import List
import Prelude ( (*), (+), (-) )


main =
   mergeMany
      [ melodyTrack ,
        patternChannel
          ( ping ++
            controller decayCC 30 ++
            controller releaseCC 30 ++
            controller volumeCC 80 ++
            marimbaLoop ) ,
        bassChannel
          ( ping ++
            controller brightnessCC 60 ++
            controller volumeCC 80 ++
            bassLoop ) ] ;

melodyTrack =
   melodyChannel (
      bell ++
      controller attackCC 30 ++
      controller releaseCC 70 ++
      controller brightnessCC 0 ++
      melodyLoop
   ) ;

melodyLoop =
   melody0 ++ melody1 ++ melody2 ++ melodyLoop ;

melody0 =
   note qn (g 5) ++ note en (g 5) ++
   note qn (g 5) ++ note en (g 5) ++
   note en (a 5) ++ note en (g 5) ++
   rest en ++
   note qn (fs 5) ++ note en (a 5) ++ note hn (d 5) ++
   [] ;

melody1 =
   note qn (c 6) ++ note en (c 6) ++
   note qn (c 6) ++ note en (c 6) ++
   note en (c 6) ++ note en (c 6) ++
   rest en ++
   note qn (b 5) ++ note en (b 5) ++ note hn (g 5) ++
   [] ;

melody2 =
   note qn (d 6) ++ note en (d 6) ++
   note qn (f 6) ++ note en (f 6) ++
   note en (f 6) ++ note en (f 6) ++
   rest en ++
   note qn (e 6) ++ note en (e 6) ++
   note qn (c 6) ++ note en (c 6) ++ note en (c 6) ++
   note qn (a 5) ++ note en (a 5) ++ note en (a 5) ++
   note qn (a 5) ++ note qn (a 5) ++
   rest qn ++
   note qn (g 5) ++ note qn (g 5) ++ rest qn ++
   [] ;


marimbaLoop =
   abba marimbaPattern0 marimbaPattern1 ++
   acba marimbaPattern0 marimbaPattern1 marimbaPattern2 ++
   marimbaLoop ;

marimbaPattern0 =
   marimbaPattern (g 4) (e 4) (d 4) ;

marimbaPattern1 =
   marimbaPattern (fs 4) (e 4) (d 4) ;

marimbaPattern2 =
   marimbaPattern (e 4) (d 4) (c 4) ;

marimbaPattern p0 p1 p2 =
   rest en ++
   note qn p0 ++ note en p0 ++
   note en p0 ++ note en p0 ++
   note en p1 ++ note en p2 ;

bassLoop =
   abba bassPattern0 bassPattern1 ++
   acba bassPattern0 bassPattern1 bassPattern2 ++
   bassLoop ;

bassPattern0 =
   bassPattern (g 2) (b 2) (d 3) ;

bassPattern1 =
   bassPattern (d 2) (fs 2) (a 2) ;

bassPattern2 =
   bassPattern (c 2) (e 2) (g 2) ;

bassPattern p0 p1 p2 =
   note dqn p0 ++ note dqn p1 ++ note qn p2 ;

abba a b =
   a ++ b ++ b ++ a ;

acba a b c =
   a ++ c ++ b ++ a ;


-- * concatenation

double x = concat [ x, x ] ;

quad x = concat [ x, x, x, x ] ;

quadAlt x y = concat [ x, x, x, y] ;


-- * durations

en = 170 ;
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
