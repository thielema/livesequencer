module Maybe where

import List (map);
import ListLive (cons);
import Function (id, ($), (.));
import Prelude ();

data Maybe a = Nothing | Just a;


maybe :: b -> (a -> b) -> Maybe a -> b;
maybe x _ Nothing = x;
maybe _ f (Just a) = f a;

fromMaybe :: a -> Maybe a -> a;
fromMaybe a = maybe a id;

catMaybes :: [Maybe a] -> [a];
catMaybes [] = [];
catMaybes (mx : xs) =
  maybe id cons mx $ catMaybes xs;

mapMaybe :: (a -> Maybe b) -> [a] -> [b];
mapMaybe f = catMaybes . map f;
