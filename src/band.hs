main =  merge 
        ( repeat chords ) ( repeat drums ) ;

oc = 100 ;
qu = times 2 oc ;
ha = times 2 qu ;
ga = times 2 ha ;

chords = concat [ quad ( dur qu 0 c 64 ) 
       	 	, quad ( moll qu 0 a 64 ) 
       	 	, quad ( dur qu 0 f 64 )
		, quad ( dur7 qu 0 g 64 )
		] ;

drums = concat 
        [ note ha 9 36 80 
        , quad ( note oc 9 38 64 )  
        ]  ;

quad x = concat [ x, x,x , x ] ;

