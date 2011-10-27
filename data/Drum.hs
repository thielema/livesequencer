module Drum where

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

chords =
    channel 0 ( concat
                [ quad ( major qn (c 4) )
                , quad ( minor qn (a 4) )
                , quad ( major qn (f 4) )
                , quad ( major7 qn (g 4) )
                ] ) ;

ifthenelse True  y n = y ;
ifthenelse False y n = n ;

quad x = concat [x,x,x,x] ;

drums =
    channel 9 ( concat
        [ emphasize 16 ( note hn 36 )
        , concat [ ifthenelse ( checkBox B5 True ) ( note en 38 ) [ Wait en ]
                 , ifthenelse ( checkBox B6 False ) ( note en 38 ) [ Wait en ]
                 , ifthenelse ( checkBox B7 False ) ( note en 38 ) [ Wait en ]
                 , ifthenelse ( checkBox B8 True ) ( note en 38 ) [ Wait en ]
                 ]
        ] ) ;


