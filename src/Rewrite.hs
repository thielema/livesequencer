module Rewrite where

import Term
import Rule
import Program

import Control.Monad ( mzero )
import qualified Data.Map as M

-- | force head of stream:
-- evaluate until we have Cons or Nil at root,
-- then evaluate first argument of Cons fully.
force_head :: Program -> Term -> Term
force_head p t = case top p t of
    Node i [ x, xs ] | name i == "Cons"-> 
      Node i [ full p x, xs ]
    Node i [] | name i == "Nil" ->
      Node i []
    _ -> error $ "force_head: missing case for " ++ show t

-- | force full evaluation 
-- (result has only constructors and numbers)
full :: Program -> Term -> Term
full p x = 
    case top p x of
        Node f args -> Node f ( map (full p) args )
        Number n -> Number n
   
-- | evaluate until root symbol is constructor.
-- TODO: need to add tracing here
top :: Program -> Term -> Term
top p t = case t of
    Number {} -> t
    Node f xs -> 
      if isConstructor f then t 
      else top p $ eval p (rules p) t
        
eval p _ t @ ( Node i xs ) 
  | name i `elem` [ "compare", "less", "minus", "plus", "times" ] = 
      let ys = map ( full p ) xs
      in  case ( name i, ys ) of    
           -- FIXME: handling of positions is dubious
           ( "less", [ Number a, Number b] ) -> 
               Node ( Identifier { name = show (a < b), position = position i } ) []
           ( "compare", [ Number a, Number b] ) -> 
               Node ( Identifier { name = show (compare a b), position = position i } ) []
           ( "minus", [ Number a, Number b] ) -> Number $ a - b
           ( "plus", [ Number a, Number b] ) -> Number $ a + b
           ( "times", [ Number a, Number b] ) -> Number $ a * b
           
eval p [] t = error $ unwords [ "eval", show t ]
eval p (r : rs) t = 
  let Node f xs = lhs r ; Node g ys = t
  in  if f == g then
        let (m, ys') = match_expand_list p xs ys
            t' = Node g ys'
        in  case m of
               Nothing -> eval p rs t'
               Just sub -> apply sub ( rhs r )
      else eval p rs t  
            
-- | check whether term matches pattern.        
-- do some reductions if they are necessary to decide about the match.        
-- return the reduced term in the second result component.        
match_expand :: Program -> Term -> Term -> ( Maybe (M.Map Identifier Term) , Term )
match_expand p pat t = case pat of
  Node f [] | isVariable f -> ( Just $ M.fromList [( f, t )], t )
  Number a -> 
      let t' @ ( Number b ) = top p t
      in  if a /= b then ( Nothing, t' )    
          else ( Just M.empty, t' )               
  Node f xs ->
      let t' @ ( Node g ys ) = top p t
      in  if f /= g then ( Nothing, t' )
          else let ( m, ys' ) = match_expand_list p xs ys
               in  ( m, Node f ys' )

match_expand_list p [] [] = ( return $ M.empty, [] )
match_expand_list p (x:xs) (y:ys) =
    let (m, y') = match_expand p x y
    in  case m of
            Nothing -> ( m, y' : ys )
            Just s  -> 
              let (n, ys') = match_expand_list p xs ys
              in  case n of
                  Nothing -> ( n, y' : ys' )
                  Just s' -> ( return $ M.unionWith (error "match_expand_list: non-linear pattern") s s'
                             , y' : ys' )  

apply :: M.Map Identifier Term -> Term -> Term
apply m t = case t of
  Node f xs -> case M.lookup f m of
      Nothing -> Node f ( map ( apply m ) xs )
      Just t' -> t'
  _ -> t    
        
