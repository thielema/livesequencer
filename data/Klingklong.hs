module Klingklong where

import Chords
import Pitch
import Midi
import List
import Prelude ( (*) )


main = [] ;


-- * patterns

loop0 = append ( patternChannel ( note qn (c 4) ) ) loop0 ;

loop0_1 =
    append ( patternChannel (
        append ( program 0 ) ( note qn (c 4) )
    ) ) loop0_1 ;


pattern1 = patternChannel ( concat [
  note qn (e 4), note qn (c 4),
  note qn (e 4), note qn (c 4),
  note qn (g 4), note en (f 4), note en (e 4),
  note en (d 4), note en (e 4), note en (f 4),
    note en (d 4) ] ) ;

pattern2 = concat [
  note qn (e 4), note qn (a 4),
  note qn (e 4), note qn (a 4),
  note qn (a 4), note en (b 4), note en (c 5),
  note en (d 5), note en (c 5), note en (b 4) ] ;

pattern2a =
  patternChannel ( concat [pattern2, note en (a 4) ] ) ;

pattern2b =
  patternChannel ( concat [pattern2, note en (g 4) ] ) ;


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


loop1 = append pattern1 loop1 ;

loop2a = append pattern2a loop2a ;

loop2b = append pattern2b loop1 ;


-- * pad

padSimple = padChannel ( concat [
  program 4,
  controller volumeCC 90, controller brightnessCC 60,
  chord4 wn2 (c 4) (e 4) (g 4) (c 5) ] ) ;

pad1 = padChannel ( concat [
  program 4,
  controller volumeCC 90,
  chord4 wn2 (c 4) (e 4) (g 4) (c 5),
  chord4 wn2 (c 4) (d 4) (g 4) (c 5),
  chord4 wn2 (c 4) (f 4) (a 4) (c 5),
  chord4 wn2 (c 4) (e 4) (g 4) (c 5) ] ) ;

pad2 = padChannel ( concat [
  program 4,
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
  program 4,
  controller volumeCC 90, controller brightnessCC 70,
  chord4 wn2 (c 4) (f 4) (a 4) (c 5),
  chord4 wn2 (e 4) (a 4) (c 5) (e 5),
  chord4 wn2 (d 4) (g 4) (b 4) (d 5),
  chord4 wn2 (d 4) (fs 4) (a 4) (d 5) ] ) ;


-- * bass

bassControl =
  controlCurve en brightnessCC
    (take 64 (cycle [0,31,62,93,124] ) ) ;

bassNote p = bassChannel ( concat [
  program 9,
  controller volumeCC 100,
  merge ( note (4*2*wn) p ) bassControl ] ) ;

bass3 = bassChannel ( concat [
  program 9,
  controller volumeCC 100,
  merge ( concat [ note wn2 (f 2), note wn2 (f 2),
                   note wn2 (g 2), note wn2 (a 2) ] )
        bassControl ] ) ;

bassLoop =
  append ( bassNote (c 2) ) bassLoop ;


-- * sweep

sweep =
   padChannel ( controlCurve en brightnessCC triangle) ;

triangle = append ( rampUp 0 ) ( rampDown 124 ) ;

rampUp 124 = [] ;
rampUp n = n : rampUp (n+4) ;

rampDown 0 = [] ;
rampDown n = n : rampDown (n-4) ;


-- * complex

loop1p = append ( merge pattern1 pad1 ) loop1p ;

pad3loop =
  append
    ( merge ( quadAlt pattern3a pattern3b ) pad3 )
    loop1 ;

loop3sweep =
   append ( merge pad3 sweep ) loop3sweep ;

bassPad =
  append
    ( merge pad1 (bassNote (c 2) ) )
    bassPad ;

pattern1Bass =
  append
    ( merge ( quad pattern1 ) (bassNote (c 2) ) )
    pattern1Bass ;

pattern1BassPadShort =
  append
    ( mergeMany [
        quad pattern1,
        bassNote (c 2),
        pad1, sweep
    ] )
    pattern1BassPadShort ;

pattern1BassPad =
  append
    ( double ( mergeMany [
        quad pattern1,
        bassNote (c 2),
        pad1, sweep ] ) )
    pattern2BassPad ;

pattern2BassPad =
  append (
    mergeMany [
      append
        (quad pattern2a)
        (quadAlt pattern2a pattern2b),
      double (bassNote (a 1)),
      pad2, double sweep
    ] )
    pattern1BassPad ;

pattern3BassPad =
  append
    ( mergeMany [
       quadAlt pattern3a pattern3b,
       bass3, pad3, sweep
    ] )
    pattern1BassPad ;


-- * concatenation

double x = concat [ x, x ] ;

quad x = concat [ x, x, x, x ] ;

quadAlt x y = concat [ x, x, x, y] ;


-- * durations

en = 170 ;
qn = 2 * en ;
hn = 2 * qn ;
wn = 2 * hn ;
dwn = wn + hn ;
wn2 = 2 * wn ;


-- * MIDI channels

soloChannel    xs = channel 0 xs ;
patternChannel xs = channel 1 xs ;
padChannel     xs = channel 2 xs ;
bassChannel    xs = channel 3 xs ;


-- * MIDI controllers

volumeCC = 7 ;
brightnessCC = 70 ;
