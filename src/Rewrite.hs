module Rewrite where

import Term
import Rule
import Program

import Control.Monad.Trans.Reader ( Reader, runReader, asks )
import Control.Monad.Trans.Writer ( WriterT, runWriterT, runWriter, tell )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Exception.Synchronous
           ( Exceptional, ExceptionalT, runExceptionalT, throwT )
import qualified Data.Map as M
import qualified Data.Traversable as Trav

import Data.Maybe.HT ( toMaybe )
import Data.List ( intercalate )


data Message = Step { target :: Identifier
                    , rule :: Maybe Identifier -- ^ Nothing for builtins
                    }
             | Data { origin :: Identifier }
    deriving Show

type Evaluator =
    ExceptionalT (Range, String) ( WriterT [ Message ] ( Reader Program ) )

runEval ::
    Evaluator a -> Program ->
    (Exceptional (Range, String) a, [ Message ])
runEval evl p =
    flip runReader p . runWriterT . runExceptionalT $ evl

exception :: Range -> String -> Evaluator a
exception rng msg =
    throwT $ (rng, msg)


-- | force head of stream:
-- evaluate until we have Cons or Nil at root,
-- then evaluate first argument of Cons fully.
force_head :: Term -> Evaluator Term
force_head t = do
    t' <- top t
    case t' of
      Node i [ x, xs ] | name i == ":" -> do
        y <- full x
        return $ Node i [ y, xs ]
      Node i [] | name i == "[]" ->
        return $ Node i []
      _ ->
        exception (termRange t') $ "not a list term: " ++ show t

-- | force full evaluation
-- (result has only constructors and numbers)
full :: Term -> Evaluator Term
full x = do
    x' <- top x
    case x' of
        Node f args ->
            fmap (Node f) $ mapM full args
        Number _ _ -> return x'

-- | evaluate until root symbol is constructor.
top :: Term -> Evaluator Term
top t = case t of
    Number {} ->
        return t
    Node f xs ->
        if isConstructor f
          then return t
          else do
              rs <- lift $ lift $ asks rules
              eval rs f xs  >>=  top

-- | do one reduction step at the root
eval :: [ Rule ] -> Identifier -> [Term] -> Evaluator Term
eval _ i xs
  | name i `elem` [ "compare", "<", "-", "+", "*" ] = do
      ys <- mapM top xs
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

eval [] i xs =
    exception (range i) $ unwords [ "cannot reduce", show $ Node i xs ]
eval (r : rs) g ys =
    case lhs r of
        Node f xs ->
            if f == g
                then do
                    (m, ys') <- match_expand_list xs ys
                    case m of
                         Nothing -> eval rs g ys'
                         Just sub -> do
                             lift $ tell [ Step { target =  g
                                           , rule = Just $ f } ]
                             return $ apply sub ( rhs r )
                else eval rs g ys
        t ->
            exception (termRange t) $
            "left-hand side of a rule must be a function call pattern, but not "
                ++ show t

-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
match_expand ::
    Term -> Term ->
    Evaluator ( Maybe (M.Map Identifier Term) , Term )
match_expand pat t = case pat of
    Node f [] | isVariable f -> do
        return ( Just $ M.singleton f t , t )
    Number _ a -> do
        t' @ ( Number _ b ) <- top t
        return ( toMaybe (a==b) M.empty, t' )
    Node f xs -> do
        t' @ ( Node g ys ) <- top t
        if f /= g
            then return ( Nothing, t' )
            else do
                 ( m, ys' ) <- match_expand_list xs ys
                 return ( m, Node f ys' )


match_expand_list ::
    [Term] ->
    [Term] ->
    Evaluator (Maybe (M.Map Identifier Term), [Term])
match_expand_list [] [] = return ( Just M.empty, [] )
match_expand_list (x:xs) (y:ys) = do
    (m, y') <- match_expand x y
    case m of
        Nothing -> return ( m, y' : ys )
        Just s  -> do
            (n, ys') <- match_expand_list xs ys
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
match_expand_list (x:_) _ =
    exception (termRange x) "too few arguments"
match_expand_list _ (y:_) =
    exception (termRange y) "too many arguments"

apply :: M.Map Identifier Term -> Term -> Term
apply m t = case t of
    Node f xs -> case M.lookup f m of
        Nothing -> Node f ( map ( apply m ) xs )
        Just t' -> t'
    _ -> t
