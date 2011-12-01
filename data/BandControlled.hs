module BandControlled where

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
    channel 9 ( concat
        [ emphasize 16 ( note hn 36 )
        , concat [ ifThenElse ( checkBox B5 True ) ( note en 38 ) [ Wait en ]
                 , ifThenElse ( checkBox B6 False ) ( note en 38 ) [ Wait en ]
                 , ifThenElse ( checkBox B7 False ) ( note en 38 ) [ Wait en ]
                 , ifThenElse ( checkBox B8 True ) ( note en 38 ) [ Wait en ]
                 ]
        ] ) ;


