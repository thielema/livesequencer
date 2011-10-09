main = transform morse ;

transform ( Cons A xs ) = append hi ( transform xs ) ;
transform ( Cons B xs ) = append lo ( transform xs ) ;
transform ( Cons C xs ) = append mid ( transform xs ) ;

morse = Cons A ( tail ( expand morse ) ) ;

tail (Cons x xs) = xs ;

expand ( Cons A xs ) = Cons A ( Cons B ( Cons C (expand xs ))) ;
expand ( Cons B xs ) = Cons A ( Cons C ( expand xs )) ;
expand ( Cons C xs ) = Cons A (  expand xs ) ;

append Nil y = y ;
append (Cons x y) z = Cons x (append y z) ;

hi = note 70 200 ;
mid = note 64 300 ;
lo = note 60 200 ;


note x y =
  Cons (On x) (Cons (Wait y) (Cons (Off x) Nil)) ;


