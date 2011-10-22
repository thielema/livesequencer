module Simplesong where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = repeat ( merge voice1 voice2 ) ;

qn = 600 ;
hn = 2 * qn ;

vel = 64 ;

voice1 =
    channel 0 (concat [ program 0 , note qn (c 4) vel , note hn (ds 4) vel , note qn (gs 4) vel ] ) ;

voice2 =
    channel 1 (concat [ program 1 , note hn (gs 5) vel , note hn (as 5) vel ] ) ;
