main = repeat ( merge voice1 voice2 ) ;

qu = 600 ;
ha = times 2 qu ;

voice1 =
    channel 0 (concat [ program 0 , note qu 60 64 , note ha 63 64 , note qu 68 64 ] ) ;

voice2 =
    channel 1 (concat [ program 1 , note ha 80 64 , note ha 82 64 ] ) ;
