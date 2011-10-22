module Band where

import Chords
import Pitch
import Midi
import List
import Prelude ( (*) )

main = merge ( repeat chords ) ( repeat drums ) ;

en = 100 ;
qn = 2 * en ;
hn = 2 * qn ;
wn = 2 * hn ;

vel = 64 ;

chords =
    channel 0 ( concat
                [ quad ( major qn (c 4) vel )
                , quad ( minor qn (a 4) vel )
                , quad ( major qn (f 4) vel )
                , quad ( major7 qn (g 4) vel )
                ] ) ;

drums =
    channel 9 ( concat
        [ note hn 36 80
        , quad ( note en 38 vel )
        ] ) ;

quad x = concat [ x, x, x, x ] ;
