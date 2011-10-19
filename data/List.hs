module List where

replicate 0 x = [] ;
replicate n x = x : replicate ( n - 1 ) x  ;

repeat s = append s (repeat s) ;

append [] ys = ys ;
append (x : xs) ys = x : append xs ys ;

concat [] = [] ;
concat (x : xs) = append x (concat xs) ;

merge (Wait a : xs) (Wait b : ys) =
  mergehelper (compare a b) a xs b ys ;
merge (Wait a : xs) (y : ys) =
  y : merge (Wait a : xs) ys ;
merge (x : xs) (Wait b : ys) =
  x : merge xs (Wait b : ys) ;
merge (x : xs) ys = x : merge xs ys ;
merge [] ys = ys ;

mergehelper LT  a xs b ys =
  Wait a : merge xs (Wait (b - a) : ys) ;
mergehelper EQ  a xs b ys =
  Wait a : merge xs ys ;
mergehelper GT a xs b ys =
  Wait b : merge (Wait (a - b) : xs) ys ;
