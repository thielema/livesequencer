main = repeat ( merge voice1 voice2 ) ;

q = 600 ;
h = times 2 q ;

voice1 = concat [ [ PgmChange 0 0 0 ] , note q 0 60 64 , note h 0 63 64 , note q 0 68 64 ] ;

voice2 = concat [ [ PgmChange 1 1 1 ] , note h 0 80 64 , note h 0 82 64  ]  ;


