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

vel = 64 ;

song = concat
    [ merge part1 mel, part2, part3 ]  ;

mel = channel 3 ( concat
     [ note hn c vel
      , note hn f vel, note hn e vel, note hn c vel ] ) ;

part1 = twice ( channel 0 ( concat
    [ quad ( dur qn c vel )
    , quad ( dur qn c vel )
    , quad ( dur qn c vel )
    , concat [ dur qn c vel
      	     , dur qn g vel
	     , dur qn g vel
	     , dur qn g vel
             ]
    ] ) ) ;

part2 = twice ( channel 0 ( concat
    [ twice ( quad ( moll qn d vel ) )
    , quad ( dur qn f vel )
    , twice ( moll qn e vel )
    , twice ( moll qn d vel )
    , quad ( quad ( dur qn c vel ) )
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
