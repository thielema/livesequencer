module Simplesong where

import Pitch
import Midi
import List
import Prelude ( (*) )


main, voice1, voice2 :: [Event (Channel Message)] ;
main = cycle ( merge voice1 voice2 ) ;

qn, hn :: Time ;
qn = 600 ;
hn = 2 * qn ;

voice1 =
    channel 0 (concat [ program 0 , note qn (c 4) , note hn (ds 4) , note qn (gs 4) ] ) ;

voice2 =
    channel 1 (concat [ program 1 , note hn (gs 5) , note hn (as 5) ] ) ;
