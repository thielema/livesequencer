module Klingklong where

import Chords
import Pitch
import Midi
import List
import Prelude ( Integer, (*), (+), (-) )


main :: [Midi.Event (Midi.Channel Midi.Message)] ;
main = [] ;


-- * patterns

loop0, loop0_1, loop1, loop2a, loop2b
   :: [Midi.Event (Midi.Channel Midi.Message)] ;
loop0 = patternChannel ( note qn (c 4) ) ++ loop0 ;

loop0_1 =
    patternChannel (
        ping ++ note qn (c 4)
    ) ++ loop0_1 ;


pattern1, pattern2a, pattern2b, pattern3a, pattern3b
   :: [Midi.Event (Midi.Channel Midi.Message)] ;
pattern1 = patternChannel ( concat [
  note qn (e 4), note qn (c 4),
  note qn (e 4), note qn (c 4),
  note qn (g 4), note en (f 4), note en (e 4),
  note en (d 4), note en (e 4), note en (f 4),
    note en (d 4) ] ) ;

pattern2 :: [Midi.Event Midi.Message] ;
pattern2 = concat [
  note qn (e 4), note qn (a 4),
  note qn (e 4), note qn (a 4),
  note qn (a 4), note en (b 4), note en (c 5),
  note en (d 5), note en (c 5), note en (b 4) ] ;

pattern2a =
  patternChannel ( pattern2 ++ note en (a 4) ) ;

pattern2b =
  patternChannel ( pattern2 ++ note en (g 4) ) ;


pattern3a = patternChannel ( concat [
  note qn (e 4), note qn (c 4),
  note qn (e 4), note qn (c 4),
  note qn (g 4), note en (f 4), note en (e 4),
  note en (d 4), note en (e 4), note en (f 4),
    note en (d 4) ] ) ;

pattern3b = patternChannel ( concat [
  note qn (e 4), note qn (c 4),
  note qn (e 4), note qn (c 4),
  note qn (e 4), note en (d 4), note en (c 4),
  note en (d 4), note en (e 4), note en (d 4),
    note en (c 4) ] ) ;


loop1 = pattern1 ++ loop1 ;

loop2a = pattern2a ++ loop2a ;

loop2b = pattern2b ++ loop1 ;


-- * pad

padSimple, pad1, pad2, pad3,
  bass3, bassLoop,
  sweep, loop1p,
  pad3loop, loop3sweep, bassPad,
  pattern1Bass, pattern1BassPadShort,
  pattern1BassPad, pattern2BassPad, pattern3BassPad
    :: [Midi.Event (Midi.Channel Midi.Message)] ;


padSimple = padChannel ( concat [
  pad,
  controller volumeCC 90, controller brightnessCC 60,
  chord4 wn2 (c 4) (e 4) (g 4) (c 5) ] ) ;

pad1 = padChannel ( concat [
  pad,
  controller volumeCC 90,
  chord4 wn2 (c 4) (e 4) (g 4) (c 5),
  chord4 wn2 (c 4) (d 4) (g 4) (c 5),
  chord4 wn2 (c 4) (f 4) (a 4) (c 5),
  chord4 wn2 (c 4) (e 4) (g 4) (c 5) ] ) ;

pad2 = padChannel ( concat [
  pad,
  controller volumeCC 90,
  chord4 wn2 (c 4) (e 4) (a 4) (c 5),
  chord4 wn2 (b 3) (e 4) (g 4) (b 4),
  chord4 wn2 (c 4) (e 4) (a 4) (c 5),
  chord4 wn2 (a 3) (c 4) (f 4) (a 4),

  chord4 wn2 (c 4) (e 4) (a 4) (c 5),
  chord4 wn2 (b 3) (e 4) (g 4) (b 4),
  chord4 wn2 (c 4) (e 4) (a 4) (c 5),
  chord4 dwn (a 3) (c 4) (f 4) (a 4),
  chord4 hn  (b 3) (d 4) (g 4) (b 4) ] ) ;

pad3 = padChannel ( concat [
  pad,
  controller volumeCC 90, controller brightnessCC 70,
  chord4 wn2 (c 4) (f 4) (a 4) (c 5),
  chord4 wn2 (e 4) (a 4) (c 5) (e 5),
  chord4 wn2 (d 4) (g 4) (b 4) (d 5),
  chord4 wn2 (d 4) (fs 4) (a 4) (d 5) ] ) ;


-- * bass

bassControl :: [Midi.Event Midi.Message] ;
bassControl =
  controlCurve en brightnessCC
    (take 64 (cycle [0,31,62,93,124] ) ) ;

bassNote :: Pitch -> [Midi.Event (Midi.Channel Midi.Message)] ;
bassNote p = bassChannel ( concat [
  bass,
  controller volumeCC 100,
  merge ( note (4*2*wn) p ) bassControl ] ) ;

bass3 = bassChannel ( concat [
  bass,
  controller volumeCC 100,
  merge ( concat [ note wn2 (f 2), note wn2 (f 2),
                   note wn2 (g 2), note wn2 (a 2) ] )
        bassControl ] ) ;

bassLoop =
  bassNote (c 2) ++ bassLoop ;


-- * sweep

sweep =
   padChannel ( controlCurve en brightnessCC triangle) ;

triangle :: [Integer] ;
triangle = rampUp 0 ++ rampDown 124 ;

rampUp, rampDown :: Integer -> [Integer] ;
rampUp 124 = [] ;
rampUp n = n : rampUp (n+4) ;

rampDown 0 = [] ;
rampDown n = n : rampDown (n-4) ;


-- * complex

loop1p = merge pattern1 pad1 ++ loop1p ;

pad3loop =
    merge ( quadAlt pattern3a pattern3b ) pad3
    ++
    loop1 ;

loop3sweep =
  merge pad3 sweep ++ loop3sweep ;

bassPad =
    merge pad1 (bassNote (c 2) )
    ++
    bassPad ;

pattern1Bass =
    merge ( quad pattern1 ) (bassNote (c 2) )
    ++
    pattern1Bass ;

pattern1BassPadShort =
    mergeMany [
        quad pattern1,
        bassNote (c 2),
        pad1, sweep
    ]
    ++
    pattern1BassPadShort ;

pattern1BassPad =
    double ( mergeMany [
        quad pattern1,
        bassNote (c 2),
        pad1, sweep ] )
    ++
    pattern2BassPad ;

pattern2BassPad =
    mergeMany [
      append
        (quad pattern2a)
        (quadAlt pattern2a pattern2b),
      double (bassNote (a 1)),
      pad2, double sweep
    ]
    ++
    pattern1BassPad ;

pattern3BassPad =
    mergeMany [
       quadAlt pattern3a pattern3b,
       bass3, pad3, sweep
    ]
    ++
    pattern1BassPad ;


-- * concatenation

double :: [a] -> [a] ;
double x = concat [ x, x ] ;

quad :: [a] -> [a] ;
quad x = concat [ x, x, x, x ] ;

quadAlt :: [a] -> [a] -> [a] ;
quadAlt x y = concat [ x, x, x, y] ;


-- * durations

en, qn, hn, wn, dwn, wn2 :: Midi.Time ;

en  = 170 ;
qn  = 2 * en ;
hn  = 2 * qn ;
wn  = 2 * hn ; dwn = wn + hn ;
wn2 = 2 * wn ;


-- * MIDI program

ping, slap, pad, bass :: [Midi.Event Midi.Message] ;
ping = program 0 ;
slap = program 2 ;
pad  = program 4 ;
bass = program 9 ;


-- * MIDI channels

soloChannel, patternChannel, padChannel, bassChannel
   :: [Midi.Event a] -> [Midi.Event (Midi.Channel a)] ;
soloChannel    xs = channel 0 xs ;
patternChannel xs = channel 1 xs ;
padChannel     xs = channel 2 xs ;
bassChannel    xs = channel 3 xs ;


-- * MIDI controllers

volumeCC, brightnessCC :: Midi.Controller ;
volumeCC = 7 ;
brightnessCC = 70 ;
