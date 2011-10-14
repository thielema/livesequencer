main = repeat ( merge voice1 voice2 ) ;

qu = 600 ;
ha = times 2 qu ;

voice1 = 
    concat [ [ Channel 0 (PgmChange 0) ] , note qu 0 60 64 , note ha 0 63 64 , note qu 0 68 64 ] ;

voice2 = 
    concat [ [ Channel 1 (PgmChange 1) ] , note ha 1 80 64 , note ha 1 82 64  ]  ;
