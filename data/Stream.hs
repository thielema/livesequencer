module Stream where

import Midi
import List

main = channel 0 ( transform morse ) ;

transform ( Cons A xs ) = append hi ( transform xs ) ;
transform ( Cons B xs ) = append lo ( transform xs ) ;
transform ( Cons C xs ) = append mid ( transform xs ) ;

morse = Cons A ( tail ( expand morse ) ) ;

tail (Cons x xs) = xs ;

expand ( Cons A xs ) = Cons A ( Cons B ( Cons C (expand xs ))) ;
expand ( Cons B xs ) = Cons A ( Cons C ( expand xs )) ;
expand ( Cons C xs ) = Cons A (  expand xs ) ;

hi = note 200 72 64 ;
mid = note 300 60 64 ;
lo = note 400 50 64 ;
