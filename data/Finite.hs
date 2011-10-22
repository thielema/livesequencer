module Finite where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = voice ;

qn = 600 ;
hn = 2 * qn ;

vel = 64 ;

voice =
    channel 0 (concat [ program 0 , note qn (c 4) vel , note hn (ds 4) vel , note qn (gs 4) vel ] ) ;
