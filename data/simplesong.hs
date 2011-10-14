main = repeat ( merge voice1 voice2 ) ;

qu = 600 ;
ha = times 2 qu ;

voice1 = 
    concat [ [ PgmChange 0 0 0 ] , note qu 0 60 64 , note ha 0 63 64 , note qu 0 68 64 ] ;

voice2 = 
    concat [ [ PgmChange 1 1 1 ] , note ha 0 80 64 , note ha 0 82 64  ]  ;


