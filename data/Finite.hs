module Finite where

import Midi
import List
import Prelude ( (*) )


main = voice ;

qn = 600 ;
hn = 2 * qn ;

vel = 64 ;

voice =
    channel 0 (concat [ program 0 , note qn 60 vel , note hn 63 vel , note qn 68 vel ] ) ;
