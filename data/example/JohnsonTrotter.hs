module JohnsonTrotter where

import Midi ;
import Pitch ;
import List (inits, tails) ;
import ListLive ;
{-
import List ;
import Function ;
import Prelude ( Integer, (*), (+), mod ) ;
-}


main :: [ Event (Channel Message) ] ;
main =
   channel 0 $ changeTempo timeUnit $ cycle $
   (concatMap (addBass . concatMap (note qn . makePitch)) $
    johnsonTrotter indexes) ;

addBass :: [Event Message] -> [Event Message] ;
addBass xs =
   noteOn (makePitch 0) :
   xs ++
   noteOff (makePitch 0) :
   [] ;

indexes :: [Integer] ;
indexes = [1,2,3,4] ;

makePitch :: Integer -> Pitch ;
makePitch 0 = c 2 ;
makePitch 1 = c 4 ;
makePitch 2 = e 4 ;
makePitch 3 = g 4 ;
makePitch _ = c 5 ;


-- * Enumerate permutations

{- |
Enumerate permutations with only one swap of adjacent elements
between two successuve permutations.
-}
johnsonTrotter :: [a] -> [[a]] ;
johnsonTrotter [] = [[]] ;
johnsonTrotter (x:xs) =
   concat $
   zipWith id
      (cycle [walkRight x, walkLeft x])
      (johnsonTrotter xs) ;

{- does not reduce term size
johnsonTrotterInt :: [Integer] -> [[Integer]] ;
johnsonTrotterInt [] = [[]] ;
johnsonTrotterInt (x:xs) =
   concat $
   zipWith applyStrictList
      (cycle [walkRight x, walkLeft x])
      (johnsonTrotterInt xs) ;
-}

{-
reduces term size,
but easily exceeds number of allowed reductions
-}
johnsonTrotterInt :: [Integer] -> [[Integer]] ;
johnsonTrotterInt [] = [[]] ;
johnsonTrotterInt (x:xs) =
   concat $
   applyStrictListList
      (zipWith id
         (cycle [walkRight x, walkLeft x]))
      (johnsonTrotterInt xs) ;

walkLeft :: a -> [a] -> [[a]] ;
walkLeft x xs = reverse (walkRight x xs) ;

walkRight :: a -> [a] -> [[a]] ;
walkRight x xs =
  zipWith (insert x) (inits xs) (tails xs) ;

insert :: a -> [a] -> [a] -> [a] ;
insert x prefix suffix = prefix ++ x : suffix ;

-- * auxiliary

timeUnit :: Time ;
timeUnit = 150 ;

qn :: Integer ;
qn = 1 ;
