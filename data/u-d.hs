main = merge
        ( repeat song ) ( repeat drums ) ;

oc = 100 ;
qu = times 2 oc ;
ha = times 2 qu ;
ga = times 2 ha ;

song = concat
    [ merge part1 mel, part2, part3 ]  ;

mel = channel 3 ( concat
     [ note ha c 64
      , note ha f 64, note ha e 64, note ha c 64 ] ) ;

part1 = twice ( channel 0 ( concat
    [ quad ( dur qu c 64 )
    , quad ( dur qu c 64 )
    , quad ( dur qu c 64 )
    , concat [ dur qu c 64
      	     , dur qu g 64
	     , dur qu g 64
	     , dur qu g 64
             ]
    ] ) ) ;

part2 = twice ( channel 0 ( concat
    [ twice ( quad ( moll qu d 64 ) )
    , quad ( dur qu f 64 )
    , twice ( moll qu e 64 )
    , twice ( moll qu d 64 )
    , quad ( quad ( dur qu c 64 ) )
     ] ) ) ;

part3 = [] ;

quad x = concat [ x, x, x, x ] ;
twice x = concat [ x, x];

bass = 36 ; snare = 40 ;

drums = channel 9 ( concat
        [ concat ( concat ( replicate 3
                   [ note ha bass 100
                   , note ha snare 80 ] )  )
        , concat [ note qu bass 100
                 , note ha snare 80
                 , note qu snare 80
                ]
        ] ) ;
