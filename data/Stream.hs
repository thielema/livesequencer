module Stream where

import Pitch
import Midi
import List
import Prelude ()

main = channel 0 ( transform morse ) ;

data Element = A | B | C ;

transform ( A : xs ) = append hi ( transform xs ) ;
transform ( B : xs ) = append lo ( transform xs ) ;
transform ( C : xs ) = append mid ( transform xs ) ;

morse = A : tail ( expand morse ) ;

tail (x:xs) = xs ;

expand ( A : xs ) = A : B : C : expand xs ;
expand ( B : xs ) = A : C : expand xs ;
expand ( C : xs ) = A : expand xs ;

hi = note 200 (c 5) ;
mid = note 300 (c 4) ;
lo = note 400 (d 3) ;
