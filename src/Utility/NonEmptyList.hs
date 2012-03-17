{-
See also packages NonEmpty and NonEmptyList,
where NonEmpty is not much useful functions
and NonEmptyList depends on a large number of packages.
-}
module Utility.NonEmptyList where

import Data.Foldable (Foldable, foldr, )

import qualified Prelude as P
import Prelude (Eq, Ord, Show, Functor, fmap, flip, ($), )


data T a = Cons { head :: a, tail :: [a] }
   deriving (Eq, Ord, Show)


instance Functor T where
   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable T where
   foldr f y (Cons x xs) = f x $ foldr f y xs


toList :: T a -> [a]
toList (Cons x xs) = x:xs

cons :: a -> T a -> T a
cons x0 (Cons x1 xs) = Cons x0 (x1:xs)

singleton :: a -> T a
singleton x = Cons x []

reverse :: T a -> T a
reverse (Cons x xs) =
   P.foldl (flip cons) (singleton x) xs

mapHead :: (a -> a) -> T a -> T a
mapHead f (Cons x xs) = Cons (f x) xs
