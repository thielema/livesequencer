module Pattern where

import Pitch
import Midi
import List
import Prelude ( (*) )


main = voice ;

en = 200 ;
qn = 2 * en ;
hn = 2 * qn ;

harmonies =
    [ c 4, e 4, g 4, c 5 ] :
    [ d 4, f 4, a 4, c 5 ] :
    [ b 3, d 4, g 4, b 4 ] :
    [ c 4, e 4, g 4, c 5 ] :
    [];

pattern = [0, 1, 2, 1, 2, 3, 2, 1] ;

voice =
    channel 0 (program 0 ++
        concat (
            map ( note en ) $
            zipWith index
                ( concat $ map ( replicate 8 ) $
                  cycle harmonies )
                ( cycle pattern )
        ) ) ;

index xs n = xs !! n ;
