module Pattern where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = voice ;

en = 300 ;
qn = 2 * en ;
hn = 2 * qn ;

pitches = [ c 4, e 4, g 4, c5 ] ;

voice =
    channel 0 (program 0 ++
        concat (
            map (note en . index pitches) (cycle [0,2,1,2])
        ) ) ;

index xs n = xs !! n ;
