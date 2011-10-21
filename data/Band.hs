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

vel = 64 ;

chords =
    channel 0 ( concat
                [ quad ( dur qn c vel )
                , quad ( moll qn a vel )
                , quad ( dur qn f vel )
                , quad ( dur7 qn g vel )
                ] ) ;

drums =
    channel 9 ( concat
        [ note hn 36 80
        , quad ( note en 38 vel )
        ] ) ;

quad x = concat [ x, x, x, x ] ;
