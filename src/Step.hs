module Step where

import Term
import Rule

import qualified Data.Map as M
import Control.Monad ( forM, mzero )

-- | pattern must be linear
match :: [ Identifier ] -- ^ list of variables in pattern
      -> Term -- ^ pattern
      -> Term -- ^ term to match
      -> Maybe ( M.Map Identifier Term )
match vs p t = case p of
  Number {} | p == t -> return $ M.empty
  Node v [] | v `elem` vs -> return $ M.fromList [ (v, t) ]
  Node f xs -> case t of
      Node g ys | f == g && length xs == length ys -> do
          ms <- forM ( zip xs ys ) $ \ (x,y) -> match vs x y
          return $ M.unionsWith ( error "non-linear pattern" ) ms
      _ -> mzero


rootStep :: Rule -> Term -> Maybe Term
rootStep r t = do
   m <- match ( vars r ) ( lhs r ) t
   return $ apply m ( rhs r )
