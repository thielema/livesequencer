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

hi = note 200 0 72 64 ;
mid = note 300 0 60 64 ;
lo = note 400 0 50 64 ;


note duration channel pitch velocity =
  Cons (On channel pitch velocity )
      (Cons (Wait duration)
          (Cons (Off channel pitch velocity)
               Nil)) ;


