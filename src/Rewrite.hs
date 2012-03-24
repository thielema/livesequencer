module Rewrite where

import Term ( Term(Node, Number, StringLiteral),
              Identifier(Identifier, range, name), Range, termRange )
import Program ( Program )
import qualified Program
import qualified Term
import qualified Rule
import qualified Module

import Control.Monad.Trans.Reader ( Reader, runReader, asks )
import Control.Monad.Trans.Writer ( WriterT, runWriter, tell, mapWriterT )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( liftM2 )
import Control.Monad.Exception.Synchronous
           ( Exceptional(Exception,Success), ExceptionalT,
             mapExceptionalT, throwT )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as Trav

import Data.Maybe.HT ( toMaybe )
import Data.Tuple.HT ( mapSnd )
import Data.List ( intercalate )


data Message =
      Step { target :: Identifier }
    | Rule { rule :: Identifier }
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
forceHead :: Term -> Evaluator Term
forceHead t = do
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
        StringLiteral _ _ -> return x'

-- | evaluate until root symbol is constructor.
top :: Term -> Evaluator Term
top t = case t of
    Number {} -> return t
    StringLiteral {} -> return t
    Node f xs ->
        if Term.isConstructor f
          then return t
          else do
              rs <-
                  lift $ lift $
                  liftM2 (,)
                      ( asks Program.functions )
                      ( asks Program.constructors )
              eval rs f xs  >>=  top

-- | do one reduction step at the root
eval ::
    (Module.FunctionDeclarations, Module.ConstructorDeclarations) ->
    Identifier -> [Term] -> Evaluator Term
eval _ i xs
  | name i `elem` [ "compare", "<", "-", "+", "*", "div", "mod" ] = do
      ys <- mapM top xs
      lift $ tell $ [ Step { target = i } ]
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
                  "div" -> return $ Number (range i) $ div a b
                  "mod" -> return $ Number (range i) $ mod a b
                  opName ->
                      exception (range i) $ "unknown operation " ++ show opName
          _ -> exception (range i) $ "wrong number of arguments"

eval (funcs, conss) g ys =
    case M.lookup g funcs of
        Nothing ->
            exception (range g) $
            unwords [ "unknown function", show $ Node g ys ]
        Just rules ->
            evalDecls conss g rules ys


evalDecls ::
    Module.ConstructorDeclarations ->
    Identifier -> [ Rule.Rule ] -> [Term] -> Evaluator Term
evalDecls conss g =
    foldr
        (\(Rule.Rule f xs rhs) go ys -> do
            (m, ys') <- matchExpandList M.empty xs ys
            case m of
                Nothing -> go ys'
                Just (substitions, additionalArgs) -> do
                    lift $ tell $
                        Step g : Rule f :
                        ( map Data $ S.toList $ S.intersection conss $
                          S.fromList $ foldr constructors [] xs )
                    rhs' <- apply substitions rhs
                    appendArguments rhs' additionalArgs)
        (\ys ->
            exception (range g) $
            unwords [ "no matching pattern for function", show g,
                      "and arguments", show ys ])

constructors :: Term -> [Identifier] -> [Identifier]
constructors (Node f xs) acc =
    if Term.isConstructor f
      then f : foldr constructors acc xs
      else acc
constructors _ acc = acc

appendArguments :: Term -> [Term] -> Evaluator Term
appendArguments f xs =
    case Term.appendArguments f xs of
        Success t -> return t
        Exception e -> exception (termRange f) e


-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
matchExpand ::
    Term -> Term ->
    Evaluator ( Maybe (M.Map Identifier Term) , Term )
matchExpand pat t = case pat of
    Node f [] | Term.isVariable f ->
        return ( Just $ M.singleton f t , t )
    Node f xs | Term.isConstructor f -> do
        t' <- top t
        case t' of
            Node g ys ->
                if f /= g
                    then return ( Nothing, t' )
                    else do
                         ( m, ys' ) <- matchExpandList M.empty xs ys
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
    StringLiteral _ a -> do
        t' <- top t
        case t' of
            StringLiteral _ b ->
                return ( toMaybe (a==b) M.empty, t' )
            _ ->
                exception (termRange t') $
                "string pattern matched against non-string term: " ++ show t'


matchExpandList ::
    M.Map Identifier Term ->
    [Term] ->
    [Term] ->
    Evaluator (Maybe (M.Map Identifier Term, [Term]), [Term])
matchExpandList s [] ys = return ( Just (s,ys), ys )
matchExpandList s (x:xs) (y:ys) = do
    (m, y') <- matchExpand x y
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
                matchExpandList s'' xs ys
matchExpandList _ (x:_) _ =
    exception (termRange x) "too few arguments"

apply :: M.Map Identifier Term -> Term -> Evaluator Term
apply m t = case t of
    Node f xs -> do
        ys <- mapM ( apply m ) xs
        case M.lookup f m of
            Nothing -> return $ Node f ys
            Just t' -> appendArguments t' ys
    _ -> return t
