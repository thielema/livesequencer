module Rewrite where

import Term ( Term(..), Identifier(..), Range(..), termRange )
import Program
import qualified Term
import qualified Rule
import qualified Module

import Control.Monad.Trans.Reader ( Reader, runReader, asks )
import Control.Monad.Trans.Writer ( WriterT, runWriter, tell, mapWriterT )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Exception.Synchronous
           ( Exceptional(Exception,Success), ExceptionalT,
             mapExceptionalT, throwT )
import qualified Data.Map as M
import qualified Data.Traversable as Trav

import Data.Maybe.HT ( toMaybe )
import Data.Tuple.HT ( mapSnd )
import Data.List ( intercalate )


data Message = Step { target :: Identifier
                    , rule :: Maybe Identifier -- ^ Nothing for builtins
                    }
             | Data { origin :: Identifier }
    deriving Show

type Evaluator =
    ExceptionalT (Range, String) ( WriterT [ Message ] ( Reader Program ) )

runEval ::
    (Monad m) =>
    Program -> Evaluator a ->
    ExceptionalT (Range, String) ( WriterT [ Message ] m ) a
runEval p =
    mapExceptionalT (mapWriterT (return . flip runReader p))

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
        String_Literal _ _ -> return x'

-- | evaluate until root symbol is constructor.
top :: Term -> Evaluator Term
top t = case t of
    Number {} -> return t
    String_Literal {} -> return t
    Node f xs ->
        if Term.isConstructor f
          then return t
          else do
              rs <- lift $ lift $ asks functions
              eval rs f xs  >>=  top

-- | do one reduction step at the root
eval :: Module.FunctionDeclarations -> Identifier -> [Term] -> Evaluator Term
eval _ i xs
  | name i `elem` [ "compare", "<", "-", "+", "*", "/", "%" ] = do
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
                  "/" -> return $ Number (range i) $ div a b
                  "%" -> return $ Number (range i) $ mod a b
                  opName ->
                      exception (range i) $ "unknown operation " ++ show opName
          _ -> exception (range i) $ "wrong number of arguments"

eval funcs g ys =
    case M.lookup g funcs of
        Nothing ->
            exception (range g) $
            unwords [ "unknown function", show $ Node g ys ]
        Just rules ->
            eval_decls g rules ys


eval_decls :: Identifier -> [ Rule.Rule ] -> [Term] -> Evaluator Term
eval_decls g =
    foldr
        (\(Rule.Rule f xs rhs) go ys -> do
            (m, ys') <- match_expand_list M.empty xs ys
            case m of
                Nothing -> go ys'
                Just (substitions, additionalArgs) -> do
                    lift $ tell [ Step { target = g
                                  , rule = Just f } ]
                    rhs' <- apply substitions rhs
                    appendArguments rhs' additionalArgs)
        (\ys ->
            exception (range g) $
            unwords [ "no matching pattern for function", show g,
                      "and arguments", show ys ])

appendArguments :: Term -> [Term] -> Evaluator Term
appendArguments f xs =
    case Term.appendArguments f xs of
        Success t -> return t
        Exception e -> exception (termRange f) e


-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
match_expand ::
    Term -> Term ->
    Evaluator ( Maybe (M.Map Identifier Term) , Term )
match_expand pat t = case pat of
    Node f [] | Term.isVariable f ->
        return ( Just $ M.singleton f t , t )
    Node f xs | Term.isConstructor f -> do
        t' <- top t
        case t' of
            Node g ys ->
                if f /= g
                    then return ( Nothing, t' )
                    else do
                         ( m, ys' ) <- match_expand_list M.empty xs ys
                         return ( fmap fst m, Node f ys' )
            _ ->
                exception (termRange t') $
                "constructor pattern matched against non-constructor term: " ++ show t'
    Node _ _ ->
        exception (termRange pat) $
            "pattern is neither constructor nor number: " ++ show pat
    Number _ a -> do
        t' <- top t
        case t' of
            Number _ b ->
                return ( toMaybe (a==b) M.empty, t' )
            _ ->
                exception (termRange t') $
                "number pattern matched against non-number term: " ++ show t'
    String_Literal _ a -> do
        t' <- top t
        case t' of
            String_Literal _ b ->
                return ( toMaybe (a==b) M.empty, t' )
            _ ->
                exception (termRange t') $
                "string pattern matched against non-string term: " ++ show t'


match_expand_list ::
    M.Map Identifier Term ->
    [Term] ->
    [Term] ->
    Evaluator (Maybe (M.Map Identifier Term, [Term]), [Term])
match_expand_list s [] ys = return ( Just (s,ys), ys )
match_expand_list s (x:xs) (y:ys) = do
    (m, y') <- match_expand x y
    case m of
        Nothing -> return ( Nothing, y' : ys )
        Just s' -> do
            s'' <-
                case runWriter $ Trav.sequenceA $
                     M.unionWithKey (\var t _ -> tell [var] >> t)
                         (fmap return s) (fmap return s') of
                    (un, []) -> return $ un
                    (_, vars) -> exception (termRange y') $
                        "variables bound more than once in pattern: " ++
                        intercalate ", " (map name vars)
            fmap (mapSnd (y':)) $
                match_expand_list s'' xs ys
match_expand_list _ (x:_) _ =
    exception (termRange x) "too few arguments"

apply :: M.Map Identifier Term -> Term -> Evaluator Term
apply m t = case t of
    Node f xs -> do
        ys <- mapM ( apply m ) xs
        case M.lookup f m of
            Nothing -> return $ Node f ys
            Just t' -> appendArguments t' ys
    _ -> return t
