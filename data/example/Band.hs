module Band where

import Drum
import Chord
import Pitch
import Midi
import List
import Prelude ( (*) ) ;

main, chords, drums :: [Event (Channel Message)] ;
main = merge ( cycle chords ) ( cycle drums ) ;

en, qn, hn, wn :: Time ;
en = 100 ;
qn = 2 * en ;
hn = 2 * qn ;
wn = 2 * hn ;

chords =
    channel 0 ( concat
                [ quad ( major qn (c 4) )
                , quad ( minor qn (a 4) )
                , quad ( major qn (f 4) )
                , quad ( major7 qn (g 4) )
                ] ) ;

drums =
    drumChannel ( concat
        [ emphasize 16 ( drum bassDrum1 hn )
        , quad ( drum acousticSnare en )
        ] ) ;

quad :: [a] -> [a] ;
quad x = concat [ x, x, x, x ] ;
