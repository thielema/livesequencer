module Sweep where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = channel 0 ( merge control voice ) ;

wn = 2000 ;

vel = 64 ;

chord dur p0 p1 p2 p3 v =
    merge ( note dur p0 v )
        ( merge ( note dur p1 v )
            ( merge ( note dur p2 v )
                ( note dur p3 v ) )) ;

voice =
    append
        ( program 4 )
        ( repeat
            ( concat [
                chord wn (c 4) (e 4) (g 4) (c 5) vel,
                chord wn (b 3) (e 4) (g 4) (b 4) vel,
                chord wn (c 4) (f 4) (a 4) (c 5) vel ] ) ) ;

cutoff = 70 ;

control = controlCurve 50 cutoff ( repeat triangle ) ;

triangle = append ( rampUp 0 ) ( rampDown 127 ) ;

rampUp 127 = [] ;
rampUp n = n : rampUp (n+1) ;

rampDown 0 = [] ;
rampDown n = n : rampDown (n-1) ;
