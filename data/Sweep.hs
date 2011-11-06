module Sweep where

import Chords
import Pitch
import Midi
import List
import Prelude ( (*), (+), (-) )


main = channel 0 ( merge control voice ) ;

wn = 2000 ;

voice =
    append
        ( program 4 )
        ( repeat
            ( concat [
                chord4 wn (c 4) (e 4) (g 4) (c 5),
                chord4 wn (b 3) (e 4) (g 4) (b 4),
                chord4 wn (c 4) (f 4) (a 4) (c 5) ] ) ) ;

cutoff = 70 ;

control = controlCurve 50 cutoff ( repeat triangle ) ;

triangle = append ( rampUp 0 ) ( rampDown 127 ) ;

rampUp 127 = [] ;
rampUp n = n : rampUp (n+1) ;

rampDown 0 = [] ;
rampDown n = n : rampDown (n-1) ;
