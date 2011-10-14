main = merge
        ( repeat song ) ( repeat drums ) ;

oc = 100 ;
qu = times 2 oc ;
ha = times 2 qu ;
ga = times 2 ha ;

song = concat
    [ merge part1 mel, part2, part3 ]  ;

mel = concat
     [ note ha 3 c 64
      , note ha 3 f 64, note  ha 3 e 64, note ha 3 c 64 ] ;

part1 = twice ( concat
    [ quad ( dur qu 0 c 64 )
    , quad ( dur qu 0 c 64 )
    , quad ( dur qu 0 c 64 )
    , concat [ dur qu 0 c 64
      	     , dur qu 0 g 64
	     , dur qu 0 g 64
	     , dur qu 0 g 64
             ]
    ] ) ;

part2 = twice ( concat
    [ twice ( quad ( moll qu 0 d 64 ) )
    , quad ( dur  qu 0 f 64 )
    , twice ( moll qu 0 e 64 )
    , twice ( moll qu 0 d 64 )
    , quad ( quad ( dur qu 0 c 64 ) )
     ] ) ;

part3 = [] ;

quad x = concat [ x, x,x , x ] ;
twice x = concat [ x, x];

bass = 36 ; snare = 40 ;

drums = concat
        [ concat ( concat ( replicate 3
                   [ note ha 9 bass 100
                   , note ha 9 snare 80 ] )  )
        , concat [ note qu 9 bass 100
                 , note ha 9 snare 80
                 , note qu 9 snare 80
                ]
        ]  ;

