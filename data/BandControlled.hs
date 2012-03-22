module BandControlled where

import Controls
import Drum
import Chords
import Pitch
import Midi
import List
import Bool
import Prelude ( (*), Bool(False, True) )

main, chords, drums :: [Event (Channel Message)] ;
main = merge ( cycle chords ) ( cycle drums ) ;

en, qn, hn :: Time ;
en = 100 ;
qn = 2 * en ;
hn = 2 * qn ;

chords =
    channel 0 ( concat
                [ quad ( major qn (c 4) )
                , quad ( minor qn (a 4) )
                , quad ( major qn (f 4) )
                , quad ( major7 qn (g 4) )
                ] ) ;

quad :: [a] -> [a] ;
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

optDrum :: Bool -> Drum -> Time -> [Event Message] ;
optDrum sel drm dur =
    ifThenElse sel ( drum drm dur ) ( rest dur ) ;
