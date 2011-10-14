import Prelude hiding (replicate, repeat, concat)

data List a = Nil | Cons a (List a)

data Event a = Wait Integer | Event a

data Channel a = Channel Integer a

data Message =
     PgmChange Integer
   | On Integer Integer
   | Off Integer Integer

minus, plus, times :: Integer -> Integer -> Integer
minus = (-)
plus = (+)
times = (*)
