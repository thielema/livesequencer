main =  merge
        ( repeat chords ) ( repeat drums ) ;

oc = 100 ;
qu = times 2 oc ;
ha = times 2 qu ;
ga = times 2 ha ;

chords =
    channel 0 ( concat
                [ quad ( dur qu c 64 )
                , quad ( moll qu a 64 )
                , quad ( dur qu f 64 )
                , quad ( dur7 qu g 64 )
                ] ) ;

drums =
    channel 9 ( concat
        [ note ha 36 80
        , quad ( note oc 38 64 )
        ] ) ;

quad x = concat [ x, x, x, x ] ;
