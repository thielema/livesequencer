module UD where

import Chords
import Pitch
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
     [ note hn (c 4) vel
      , note hn (f 4) vel, note hn (e 4) vel, note hn (c 4) vel ] ) ;

part1 = twice ( channel 0 ( concat
    [ quad ( major qn (c 4) vel )
    , quad ( major qn (c 4) vel )
    , quad ( major qn (c 4) vel )
    , concat [ major qn (c 4) vel
      	     , major qn (g 4) vel
	     , major qn (g 4) vel
	     , major qn (g 4) vel
             ]
    ] ) ) ;

part2 = twice ( channel 0 ( concat
    [ twice ( quad ( minor qn (d 4) vel ) )
    , quad ( major qn (f 4) vel )
    , twice ( minor qn (e 4) vel )
    , twice ( minor qn (d 4) vel )
    , quad ( quad ( major qn (c 4) vel ) )
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
