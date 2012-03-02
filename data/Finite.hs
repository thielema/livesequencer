module Finite where

import Pitch
import Midi
import List
import Prelude ( (*), ($) )


main, voice :: [Event (Channel Message)] ;
main = voice ;

qn, hn :: Time ;
qn = 600 ;
hn = 2 * qn ;

voice =
    channel 0 $
        program 0 ++ note qn (c 4) ++ note hn (ds 4) ++ note qn (gs 4) ;
