main = repeat drums ;

drums = append base (append base (append base break)) ;

q = 600 ;

base = append (note q 10 10 64)
      (append (note q 10 20 64)
      (append (note q 10 30 64)
              (note q 10 40 64) ) ) ;

break = append (note q 10 20 64)
          (append (note q 10 25 64)
             (append (note q 10 25 64)
                     (note q 10 35 64) ) ) ;

repeat s = append s (repeat s) ;

append Nil ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

note duration channel pitch velocity =
  Cons (On channel pitch velocity ) 
      (Cons (Wait duration) 
          (Cons (Off channel pitch velocity) 
               Nil)) ;


