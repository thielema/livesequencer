module Rewrite where

import Term
import Rule
import Step

import Control.Monad ( mzero )

-- | compute normal form, eagerly, 
-- but never descend into second argument of Cons
normal :: [ Rule ] -> Term -> Term
normal rs t = case t of
    Node (Identifier "Cons") [ x, xs ] -> 
        let x' = normal rs x
        in  Node (Identifier "Cons") [ x', xs ]
    Node f xs ->        
        let t1 = Node f ( map ( normal rs ) xs )
        in case first_applicable_root_step rs t1 of
            Nothing -> t1
            Just t2 -> normal rs t2
    _ -> t

first_applicable_root_step :: [ Rule ] -> Term -> Maybe Term
first_applicable_root_step [] t = mzero
first_applicable_root_step (r : rs) t = 
    case root_step r t of
        Just s -> return s
        Nothing -> first_applicable_root_step rs t
        
