module Simplesong where

import Midi
import List
import Prelude ( (*) )


main = repeat ( merge voice1 voice2 ) ;

qn = 600 ;
hn = 2 * qn ;

voice1 =
    channel 0 (concat [ program 0 , note qn 60 64 , note hn 63 64 , note qn 68 64 ] ) ;

voice2 =
    channel 1 (concat [ program 1 , note hn 80 64 , note hn 82 64 ] ) ;
