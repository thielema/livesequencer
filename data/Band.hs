module Band where

import Chords
import Midi
import List
import Prelude ( (*) )

main =  merge
        ( repeat chords ) ( repeat drums ) ;

en = 100 ;
qn = 2 * en ;
hn = 2 * qn ;
wn = 2 * hn ;

chords =
    channel 0 ( concat
                [ quad ( dur qn c 64 )
                , quad ( moll qn a 64 )
                , quad ( dur qn f 64 )
                , quad ( dur7 qn g 64 )
                ] ) ;

drums =
    channel 9 ( concat
        [ note hn 36 80
        , quad ( note en 38 64 )
        ] ) ;

quad x = concat [ x, x, x, x ] ;
