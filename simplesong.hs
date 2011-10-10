main = repeat ( merge voice1 voice2 ) ;

q = 600 ;
h = times 2 q ;

voice1 = append ( note q 0 60 64 )
       ( append ( note h 0 63 64 )
                ( note q 0 68 64 ) ) ;

voice2 = append ( note h 0 80 64 )
                ( note h 0 82 64 )  ;


repeat s = append s (repeat s) ;

append Nil ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

merge (Cons (Delay a) xs) (Cons (Delay b) ys) = 
  mergehelper (less a b) a xs b ys ;
merge (Cons x xs) ys = Cons x (merge xs ys) ;
merge xs (Cons y ys) = Cons y (merge xs ys) ;
merge Nil ys = ys ; merge xs Nil = xs ;
mergehelper True  a xs b ys = 
  Cons (Delay a) (merge xs (Cons (Delay (minus b a)) ys)) ;
mergehelper False a xs b ys = 
  Cons (Delay b) (merge (Cons (Delay (minus a b)) xs) ys) ;

note duration channel pitch velocity =
  Cons (On channel pitch velocity ) 
      (Cons (Wait duration) 
          (Cons (Off channel pitch velocity) 
               Nil)) ;


