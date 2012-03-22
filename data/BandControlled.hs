module BandControlled where

import Drum
import Chords
import Pitch
import Midi
import List
import Bool
import Prelude ( (*) )

main = merge ( cycle chords ) ( cycle drums ) ;

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

quad x = concat [x,x,x,x] ;

drums =
    drumChannel ( concat
        [ emphasize 16 ( drum bassDrum1 hn )
        , concat [ optDrum ( checkBox "B5" True  ) acousticSnare en
                 , optDrum ( checkBox "B6" False ) acousticSnare en
                 , optDrum ( checkBox "B7" False ) acousticSnare en
                 , optDrum ( checkBox "B8" True  ) acousticSnare en
                 ]
        ] ) ;

optDrum b drm dur =
    ifThenElse b ( drum drm dur ) ( rest dur ) ;
