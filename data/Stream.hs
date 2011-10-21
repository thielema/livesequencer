module Stream where

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

hi = note 200 72 64 ;
mid = note 300 60 64 ;
lo = note 400 50 64 ;
