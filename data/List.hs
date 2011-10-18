module List where

data List a = Nil | Cons a (List a) ;

replicate 0 x = Nil ;
replicate n x =
  Cons x ( replicate ( minus n 1 ) x ) ;

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
merge Nil ys = ys ;

mergehelper LT  a xs b ys =
  Cons (Wait a) (merge xs (Cons (Wait (minus b a)) ys)) ;
mergehelper EQ  a xs b ys =
  Cons (Wait a) (merge xs ys) ;
mergehelper GT a xs b ys =
  Cons (Wait b) (merge (Cons (Wait (minus a b)) xs) ys) ;
