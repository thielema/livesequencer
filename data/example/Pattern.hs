module Pattern where

import Pitch
import Midi
import List
import Prelude ( Int, (*), ($) )


main, voice :: [Event (Channel Message)] ;
main = voice ;

en, qn, hn :: Time ;
en = 200 ;
qn = 2 * en ;
hn = 2 * qn ;

harmonies :: [[Pitch]] ;
harmonies =
    [ c 4, e 4, g 4, c 5 ] :
    [ d 4, f 4, a 4, c 5 ] :
    [ b 3, d 4, g 4, b 4 ] :
    [ c 4, e 4, g 4, c 5 ] :
    [];

pattern :: [ Int ] ;
pattern = [0, 1, 2, 1, 2, 3, 2, 1] ;

voice =
    channel 0 (program 0 ++
        concatMap ( note en ) (
            zipWith index
                ( concatMap ( replicate 8 ) $
                  cycle harmonies )
                ( cycle pattern )
        ) ) ;

index :: [a] -> Int -> a ;
index xs n = xs !! n ;
