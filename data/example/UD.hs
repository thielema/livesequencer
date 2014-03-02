module UD where

import Drum
import Chord
import Pitch
import Midi
import List
import Prelude ( (*) )


main, song, mel, drums :: [Event (Channel Message)] ;
main = cycle song =:= cycle drums ;

en, qn, hn, wn :: Time ;
en = 100 ;
qn = 2*en ;
hn = 2*qn ;
wn = 2*hn ;

song = concat
    [ merge part1 mel, part2, part3 ]  ;

mel = channel 3 ( concat
     [ note hn (c 4)
      , note hn (f 4), note hn (e 4), note hn (c 4) ] ) ;

part1, part2, part3 :: [Event (Channel Message)] ;
part1 = twice ( channel 0 ( concat
    [ quad ( major qn (c 4) )
    , quad ( major qn (c 4) )
    , quad ( major qn (c 4) )
    , concat [ major qn (c 4)
      	     , major qn (g 4)
	     , major qn (g 4)
	     , major qn (g 4)
             ]
    ] ) ) ;

part2 = twice ( channel 0 ( concat
    [ twice ( quad ( minor qn (d 4) ) )
    , quad ( major qn (f 4) )
    , twice ( minor qn (e 4) )
    , twice ( minor qn (d 4) )
    , quad ( quad ( major qn (c 4) ) )
     ] ) ) ;

part3 = [] ;

twice, quad :: [a] -> [a] ;
quad x = concat [ x, x, x, x ] ;
twice x = concat [ x, x];


bass, snare :: Time -> [Event Message] ;
bass dur = emphasize 36 (drum bassDrum1 dur) ;
snare dur = emphasize 16 (drum electricSnare dur) ;

drums = drumChannel ( concat
        [ concat ( concat ( replicate 3
                   [ bass hn, snare hn ] )  )
        , concat [ bass qn, snare hn, snare qn ]
        ] ) ;
