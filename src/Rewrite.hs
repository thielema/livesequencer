module Rewrite where

import Term
import Rule
import Program

import Control.Monad ( forM )
import Control.Monad.Trans.Writer ( Writer, runWriter, tell )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Exception.Synchronous ( ExceptionalT, throwT )
import qualified Data.Map as M
import qualified Data.Traversable as Trav

import Data.Maybe.HT ( toMaybe )
import Data.List ( intercalate )


data Message = Step { target :: Identifier
                    , rule :: Maybe Identifier -- ^ Nothing for builtins
                    }
             | Data { origin :: Identifier }
    deriving Show

type Evaluator = ExceptionalT (Range, String) ( Writer [ Message ] )

exception :: Range -> String -> Evaluator a
exception rng msg =
    throwT $ (rng, msg)


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
        exception (termRange t') $ "not a list term: " ++ show t

-- | force full evaluation
-- (result has only constructors and numbers)
full :: Program -> Term -> Evaluator Term
full p x = do
    x' <- top p x
    case x' of
        Node f args ->
            fmap (Node f) $ forM args $ full p
        Number _ _ -> return x'

-- | evaluate until root symbol is constructor.
top :: Program -> Term -> Evaluator Term
top p t = case t of
    Number {} ->
        return t
    Node f xs ->
        if isConstructor f
          then return t
          else eval p (rules p) f xs  >>=  top p

-- | do one reduction step at the root
eval :: Program -> [ Rule ] -> Identifier -> [Term] -> Evaluator Term
eval p _ i xs
  | name i `elem` [ "compare", "<", "-", "+", "*" ] = do
      ys <- forM xs $ top p
      lift $ tell $ [ Step { target = i, rule = Nothing } ]
      case ys of
          [ Number _ a, Number _ b] ->
              case name i of
                  -- FIXME: handling of positions is dubious
                  "<" ->
                      return $
                      Node ( Identifier { name = show (a < b)
                           , range = range i } ) []
                  "compare" ->
                      return $
                      Node ( Identifier { name = show (compare a b)
                           , range = range i } ) []
                  "-" -> return $ Number (range i) $ a - b
                  "+" -> return $ Number (range i) $ a + b
                  "*" -> return $ Number (range i) $ a * b
                  opName ->
                      exception (range i) $ "unknown operation " ++ show opName
          _ -> exception (range i) $ "wrong number of arguments"

eval _p [] i xs =
  exception (range i) $ unwords [ "cannot reduce", show $ Node i xs ]
eval p (r : rs) g ys =
    case lhs r of
        Node f xs ->
            if f == g
                then do
                    (m, ys') <- match_expand_list p xs ys
                    case m of
                         Nothing -> eval p rs g ys'
                         Just sub -> do
                             lift $ tell [ Step { target =  g
                                           , rule = Just $ f } ]
                             return $ apply sub ( rhs r )
                else eval p rs g ys
        t ->
            exception (termRange t) $
            "left-hand side of a rule must be a function call pattern, but not "
                ++ show t


-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
match_expand :: Program -> Term -> Term
             -> Evaluator ( Maybe (M.Map Identifier Term) , Term )
match_expand p pat t = case pat of
  Node f [] | isVariable f -> do
      return ( Just $ M.singleton f t , t )
  Number _ a -> do
      t' @ ( Number _ b ) <- top p t
      return ( toMaybe (a==b) M.empty, t' )
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
                        case runWriter $ Trav.sequenceA $
                             M.unionWithKey (\var t _ -> tell [var] >> t)
                                 (fmap return s) (fmap return s') of
                            (un, []) -> return $ Just un
                            (_, vars) -> exception (termRange y') $
                                "variables bound more than once in pattern: " ++
                                intercalate ", " (map name vars)
            return (n',  y' : ys')
match_expand_list _ (x:_) _ =
    exception (termRange x) "too few arguments"
match_expand_list _ _ (y:_) =
    exception (termRange y) "too many arguments"

apply :: M.Map Identifier Term -> Term -> Term
apply m t = case t of
    Node f xs -> case M.lookup f m of
        Nothing -> Node f ( map ( apply m ) xs )
        Just t' -> t'
    _ -> t
