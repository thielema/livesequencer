main =  merge ( repeat chords ) ( repeat drums ) ;

oc = 100 ;
qu = times 2 oc ;
ha = times 2 qu ;
ga = times 2 ha ;

chords = concat [ quad ( dur qu 0 c 64 ) 
       	 	, quad ( moll qu 0 a 64 ) 
       	 	, quad ( dur qu 0 f 64 )
		, quad ( dur7 qu 0 g 64 )
		] ;

drums = concat [ note ha 9 36 80 , quad ( note oc 9 38 64 )  ]  ;

quad x = concat [ x, x,x , x ] ;

c = 60 ; d = 62 ; e = 64 ; f = 65 ; g = 67 ; a = 69 ; h = 71 ;

dur delta chan base vel = 
   merge (note delta chan base vel)
       (merge (note delta chan (plus base 4) vel)
           (merge (note delta chan (plus base 7) vel)
                  ( note delta chan (plus base 12) vel)))  ;

dur7 delta chan base vel = 
   merge (note delta chan base vel)
       (merge (note delta chan (plus base 4) vel)
           (merge (note delta chan (plus base 7) vel)
                  ( note delta chan (plus base 10) vel)))  ;

moll delta chan base vel = 
   merge (note delta chan base vel)
       (merge (note delta chan (plus base 3) vel)
           (merge (note delta chan (plus base 7) vel)
                  ( note delta chan (plus base 12) vel)))  ;

moll7 delta chan base vel = 
   merge (note delta chan base vel)
       (merge (note delta chan (plus base 3) vel)
           (merge (note delta chan (plus base 7) vel)
                  ( note delta chan (plus base 10) vel)))  ;

repeat s = append s (repeat s) ;

append Nil ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

concat Nil = Nil ;
concat (Cons x xs) = append x (concat xs) ;

merge (Cons (Wait a) xs) (Cons (Wait b) ys) = 
  mergehelper (compare a b) a xs b ys ;
merge (Cons (Wait a) xs) (Cons y ys) =
  Cons y (merge (Cons (Wait a) xs) ys) ;
merge (Cons x xs) (Cons (Wait b) ys) =  
  Cons x (merge xs (Cons (Wait b) ys)) ;
merge (Cons x xs) ys = Cons x (merge xs ys) ;
merge xs (Cons y ys) = Cons y (merge xs ys) ;
merge Nil ys = ys ; merge xs Nil = xs ;

mergehelper LT  a xs b ys = 
  Cons (Wait a) (merge xs (Cons (Wait (minus b a)) ys)) ;
mergehelper EQ  a xs b ys = 
  Cons (Wait a) (merge xs ys) ;
mergehelper GT a xs b ys = 
  Cons (Wait b) (merge (Cons (Wait (minus a b)) xs) ys) ;

note duration channel pitch velocity =
  [ On channel pitch velocity 
  , Wait duration
  , Off channel pitch velocity
  ] ;
