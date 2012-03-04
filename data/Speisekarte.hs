module Speisekarte where

import Pitch
import Midi
import List
import Instrument
import Drum
import Prelude ( (*), ($), negate )

main, cascade, loop, choir :: [Event (Channel Message)] ;
main = cascade ;

cascade =
   concat $ scanl merge [] $ reverse tracks ;

loop = choir ++ loop ;
choir = mergeMany tracks ;

drumTrack, drumLoop :: [ Event (Channel Message) ] ;
drumTrack = rest dqn ++ drumLoop ;

drumLoop =
   drumChannel (
      drum bassDrum1 qn ++
      drum pedalHiHat qn ++
      drum pedalHiHat qn ++
      emphasize (negate 20) (drum electricSnare qn) ++
      drum pedalHiHat qn ++
      drum pedalHiHat qn
   ) ++ drumLoop ;

tracks :: [ [ Event (Channel Message) ] ] ;
tracks =
   sopranoTrack :
   contraltoITrack :
   contraltoIITrack :
   tenorTrack :
   bassITrack :
   bassIITrack :
   [] ;

sopranoTrack, contraltoITrack, contraltoIITrack, 
   tenorTrack, bassITrack, bassIITrack
      :: [Event (Channel Message)] ;

sopranoTrack =
   sopranoChannel (
      program flute ++
      transpose 12 sopranoMelody
   ) ;

contraltoITrack =
   contraltoIChannel (
      program harpsichord ++
      contraltoIMelody
   ) ;

contraltoIITrack =
   contraltoIIChannel (
      program acousticGrandPiano ++
      contraltoIIMelody
   ) ;

tenorTrack =
   tenorChannel (
      program stringEnsemble1 ++
      controller volumeCC 64 ++
      tenorMelody
   ) ;

bassITrack =
   bassIChannel (
      program fretlessBass ++
      bassIMelody
   ) ;

bassIITrack =
   bassIIChannel (
      program pizzicatoStrings ++
      bassIIMelody
   ) ;


vibnote :: Time -> Pitch -> [Event Message] ;
vibnote dur p =
   note dur p
   =:=
   ( [ Wait (div dur 2) ] ++
     controller modulationCC 127 ++
     [ Wait (div dur 2) ] ++
     controller modulationCC 0 );

sopranoLoop,
   sopranoMelody, contraltoIMelody, contraltoIIMelody,
   tenorMelody, bassIMelody, bassIIMelody
      :: [Event Message] ;

sopranoLoop = sopranoMelody ++ sopranoLoop ;

sopranoMelody =
   rest dqn ++
   vibnote qn (d 5) ++ note en (a 4) ++
   note en (d 5) ++ note en (d 5) ++
   note en (d 5) ++
   vibnote qn (d 5) ++ note en (a 4) ++
   note en (d 5) ++ note en (d 5) ++
   note en (d 5) ++
   vibnote qn (e 5) ++ note en (a 4) ++
   note en (e 5) ++ note en (e 5) ++
   note en (e 5) ++
   vibnote qn (e 5) ++ note en (a 4) ++
   note en (e 5) ++ note en (cis 5) ++
   note en (a 4) ++
   vibnote qn (fis 5) ++ note en (fis 5) ++
   note en (e 5) ++ note en (d 5) ++
   note en (cis 5) ++
   vibnote qn (b 4) ++ rest en ++
   note en (g 5) ++ note en (e 5) ++
   note en (b 4) ++
   note en (cis 5) ++ note en (a 4) ++
   note en (a 4) ++ note en (a 4) ++
   note en (b 4) ++ note en (cis 5) ++
   vibnote qn (d 5) ++ rest en ++
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
   note en (a 3) ++ note en (b 3) ++ note dhn (a 3) ++
   note qn (d 4) ++ note qn (fis 4) ++
   note en (e 4) ++ note en (fis 4) ++ note en (e 4) ++ note en (fis 4) ++ note en (e 4) ++ note en (d 4) ++
   note hn (cis 4) ++ rest qn ++
   note en (d 4) ++ note en (b 3) ++ note dhn (b 3) ++
   note qn (e 4) ++ note en (b 3) ++ note en (e 4) ++
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


-- * MIDI channels

sopranoChannel, contraltoIChannel, contraltoIIChannel,
   tenorChannel, bassIChannel, bassIIChannel
      :: [Event a] -> [Event (Channel a)] ;
sopranoChannel     = channel 0 ;
contraltoIChannel  = channel 1 ;
contraltoIIChannel = channel 2 ;
tenorChannel       = channel 3 ;
bassIChannel       = channel 4 ;
bassIIChannel      = channel 5 ;


-- * MIDI controllers

volumeCC, modulationCC :: Controller ;
volumeCC = 7 ;
modulationCC = 1 ;
