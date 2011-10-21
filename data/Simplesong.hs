module Simplesong where

import Midi
import List
import Prelude ( (*) )


main = repeat ( merge voice1 voice2 ) ;

qn = 600 ;
hn = 2 * qn ;

vel = 64 ;

voice1 =
    channel 0 (concat [ program 0 , note qn 60 vel , note hn 63 vel , note qn 68 vel ] ) ;

voice2 =
    channel 1 (concat [ program 1 , note hn 80 vel , note hn 82 vel ] ) ;
