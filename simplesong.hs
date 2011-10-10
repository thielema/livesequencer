main = repeat ( merge voice1 voice2 ) ;

q = 600 ;
h = times 2 q ;

voice1 = concat [ [ PgmChange 0 0 0 ] , note q 0 60 64 , note h 0 63 64 , note q 0 68 64 ] ;

voice2 = concat [ [ PgmChange 1 1 1 ] , note h 0 80 64 , note h 0 82 64  ]  ;

repeat s = append s (repeat s) ;

append Nil ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

concat Nil = Nil ;
concat (Cons x xs) = append x (concat xs) ;

merge (Cons (Wait a) xs) (Cons (Wait b) ys) = 
  mergehelper (less a b) a xs b ys ;
merge (Cons (Wait a) xs) (Cons y ys) =
  Cons y (merge (Cons (Wait a) xs) ys) ;
merge (Cons x xs) (Cons (Wait b) ys) =  
  Cons x (merge xs (Cons (Wait b) ys)) ;
merge (Cons x xs) ys = Cons x (merge xs ys) ;
merge xs (Cons y ys) = Cons y (merge xs ys) ;
merge Nil ys = ys ; merge xs Nil = xs ;

mergehelper True  a xs b ys = 
  Cons (Wait a) (merge xs (Cons (Wait (minus b a)) ys)) ;
mergehelper False a xs b ys = 
  Cons (Wait b) (merge (Cons (Wait (minus a b)) xs) ys) ;

note duration channel pitch velocity =
  [ On channel pitch velocity 
  , Wait duration
  , Off channel pitch velocity
  ] ;

