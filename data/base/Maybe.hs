module Maybe where

import Function (id);
import Prelude ();

data Maybe a = Nothing | Just a;


maybe :: b -> (a -> b) -> Maybe a -> b;
maybe x _ Nothing = x;
maybe _ f (Just a) = f a;

fromMaybe :: a -> Maybe a -> a;
fromMaybe a = maybe a id;
