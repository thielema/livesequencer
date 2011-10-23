module Rewrite where

import Term
import Rule
import Program

import qualified Text.ParserCombinators.Parsec as Pos

import Control.Monad ( forM )
import Control.Monad.Trans.Writer ( Writer, tell )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Exception.Synchronous ( ExceptionalT, throwT )
import qualified Data.Map as M
import qualified Data.Traversable as Trav


data Message = Step { target :: Identifier
                    , rule :: Maybe Identifier -- ^ Nothing for builtins
                    }
             | Data { origin :: Identifier }
    deriving Show

type Evaluator = ExceptionalT (Pos.SourcePos, String) ( Writer [ Message ] )

exception :: Term -> String -> Evaluator a
exception t msg =
    throwT $ (termPos t, msg)


-- | force head of stream:
-- evaluate until we have Cons or Nil at root,
-- then evaluate first argument of Cons fully.
force_head :: Program -> Term -> Evaluator Term
force_head p t = do
    t' <- top p t
    case t' of
      Node i [ x, xs ] | name i == ":" -> do
        y <- full p x
        return $ Node i [ y, xs ]
      Node i [] | name i == "[]" ->
        return $ Node i []
      _ ->
        exception t' $ "not a list term: " ++ show t

-- | force full evaluation
-- (result has only constructors and numbers)
full :: Program -> Term -> Evaluator Term
full p x = do
    x' <- top p x
    case x' of
        Node f args -> do
            args' <- forM args $ full p
            return $ Node f args'
        Number n -> do
            return $ Number n

-- | evaluate until root symbol is constructor.
top :: Program -> Term -> Evaluator Term
top p t = case t of
    Number {} ->
        return t
    Node f _xs ->
        if isConstructor f then return t
        else do
            e <- eval p (rules p) t
            top p e

-- | to one reduction step at the root
eval :: Program -> [ Rule ] -> Term -> Evaluator Term
eval p _ t @ ( Node i xs )
  | name i `elem` [ "compare", "<", "-", "+", "*" ] = do
      ys <- forM xs $ top p
      lift $ tell $ [ Step { target = i, rule = Nothing } ]
      case ys of
          [ Number a, Number b] ->
              case name i of
                  -- FIXME: handling of positions is dubious
                  "<" ->
                      return $
                      Node ( Identifier { name = show (a < b)
                           , start = start i, end = end i } ) []
                  "compare" ->
                      return $
                      Node ( Identifier { name = show (compare a b)
                           , start = start i, end = end i } ) []
                  "-" -> return $ Number $ a - b
                  "+" -> return $ Number $ a + b
                  "*" -> return $ Number $ a * b
                  opName ->
                      exception t $ "unknown operation " ++ show opName
          _ -> exception t $ "wrong number of arguments"

eval _p [] t = exception t $ unwords [ "cannot reduce", show t ]
eval p (r : rs) t = do
  let Node f xs = lhs r ; Node g ys = t
  if f == g
      then do
        (m, ys') <- match_expand_list p xs ys
        let t' = Node g ys'
        case m of
               Nothing -> eval p rs t'
               Just sub -> do
                   lift $ tell [ Step { target =  g
                                 , rule = Just $ f } ]
                   return $ apply sub ( rhs r )
      else eval p rs t

-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
match_expand :: Program -> Term -> Term
             -> Evaluator ( Maybe (M.Map Identifier Term) , Term )
match_expand p pat t = case pat of
  Node f [] | isVariable f -> do
      return ( Just $ M.fromList [( f, t )], t )
  Number a -> do
      t' @ ( Number b ) <- top p t
      if a /= b
          then return  ( Nothing, t' )
          else return ( Just M.empty, t' )
  Node f xs -> do
      t' @ ( Node g ys ) <- top p t
      if f /= g
          then return ( Nothing, t' )
          else do
               ( m, ys' ) <- match_expand_list p xs ys
               return ( m, Node f ys' )


match_expand_list ::
    Program ->
    [Term] ->
    [Term] ->
    Evaluator (Maybe (M.Map Identifier Term), [Term])
match_expand_list _p [] [] = return ( Just M.empty, [] )
match_expand_list p (x:xs) (y:ys) = do
    (m, y') <- match_expand p x y
    case m of
        Nothing -> return ( m, y' : ys )
        Just s  -> do
            (n, ys') <- match_expand_list p xs ys
            n' <-
                case n of
                    Nothing -> return n
                    Just s' ->
                        case Trav.sequenceA $
                             M.unionWith (\_ _ -> Nothing)
                                 (fmap Just s) (fmap Just s') of
                            Nothing -> exception y' "non-linear pattern"
                            Just un -> return $ Just un
            return (n',  y' : ys')
match_expand_list _ (x:_) _ =
    exception x "too few arguments"
match_expand_list _ _ (y:_) =
    exception y "too many arguments"

apply :: M.Map Identifier Term -> Term -> Term
apply m t = case t of
    Node f xs -> case M.lookup f m of
        Nothing -> Node f ( map ( apply m ) xs )
        Just t' -> t'
    _ -> t
