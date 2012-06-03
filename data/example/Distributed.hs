module Distributed where

import Distributed.Chord
import Distributed.Bass
import Distributed.Drum

import Pitch
import Midi
import Prelude ( (*), ($) )

main :: [Event (Channel Message)] ;
main = mergeMany $
    chordTrack en harmonies :
    bassTrack en bassNotes :
    drumTrack en :
    [] ;

en, qn, hn :: Time ;
en = 200 ;
qn = 2 * en ;
hn = 2 * qn ;

harmonies :: [[Pitch]] ;
harmonies =
    [ c 4, e 4, g 4, c 5 ] :
    [ d 4, f 4, a 4, c 5 ] :
    [ b 3, d 4, g 4, b 4 ] :
    [ c 4, e 4, g 4, c 5 ] :
    [];

bassNotes :: [[Pitch]] ;
bassNotes =
    [ c 2, d 2, e 2, f 2 ] :
    [ d 2, e 2, f 2, g 2 ] :
    [ b 1, c 2, d 2, e 2 ] :
    [ c 2, d 2, e 2, f 2 ] :
    [];
