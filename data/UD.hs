module UD where

import Chords
import Midi
import List
import Prelude ( (*) )


main = merge ( repeat song ) ( repeat drums ) ;

en = 100 ;
qn = 2*en ;
hn = 2*qn ;
wn = 2*hn ;

song = concat
    [ merge part1 mel, part2, part3 ]  ;

mel = channel 3 ( concat
     [ note hn c 64
      , note hn f 64, note hn e 64, note hn c 64 ] ) ;

part1 = twice ( channel 0 ( concat
    [ quad ( dur qn c 64 )
    , quad ( dur qn c 64 )
    , quad ( dur qn c 64 )
    , concat [ dur qn c 64
      	     , dur qn g 64
	     , dur qn g 64
	     , dur qn g 64
             ]
    ] ) ) ;

part2 = twice ( channel 0 ( concat
    [ twice ( quad ( moll qn d 64 ) )
    , quad ( dur qn f 64 )
    , twice ( moll qn e 64 )
    , twice ( moll qn d 64 )
    , quad ( quad ( dur qn c 64 ) )
     ] ) ) ;

part3 = [] ;

quad x = concat [ x, x, x, x ] ;
twice x = concat [ x, x];

bass = 36 ; snare = 40 ;

drums = channel 9 ( concat
        [ concat ( concat ( replicate 3
                   [ note hn bass 100
                   , note hn snare 80 ] )  )
        , concat [ note qn bass 100
                 , note hn snare 80
                 , note qn snare 80
                ]
        ] ) ;
