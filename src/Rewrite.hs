module Rewrite where

import Term ( Term(Node, Number, StringLiteral),
              Identifier(Identifier, range, name), Range, termRange )
import TermFocus ( TermFocus(TermFocus), SuperTerm )
import Program ( Program )
import qualified Program
import qualified TermFocus
import qualified Term
import qualified Rule

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.RWS as MRWS
import Control.Monad.Trans.RWS ( RWS, asks, tell, get, put )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Exception.Synchronous
           ( Exceptional(Exception,Success), ExceptionalT,
             mapExceptionalT, throwT, assertT )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as Trav

import Data.Monoid ( Monoid )
import Data.Maybe.HT ( toMaybe )
import Data.Tuple.HT ( mapSnd )
import Data.List ( intercalate )

-- import Debug.Trace ( trace )


data Message =
      Term { term :: TermFocus }
    | Source { source :: Source }
    deriving Show

data Source =
      Step { target :: Identifier }
    | AttemptRule { rule :: Identifier }
    | Rule { rule :: Identifier }
    | Data { origin :: Identifier }
    deriving Show

data Context =
    Context {
        maxReductions :: Count,
        program :: Program,
        superTerms :: [ SuperTerm ]
    }

type Count = Int

type Evaluator =
    ExceptionalT (Range, String) ( RWS Context [ Message ] Count )


runEval ::
    (Monad m) =>
    Count -> Program -> Evaluator a ->
    ExceptionalT (Range, String) ( MW.WriterT [ Message ] m ) a
runEval maxRed p =
    -- in transformers-0.3 you can write MW.writer instead of MW.WriterT . return
    mapExceptionalT (\evl ->
        MW.WriterT $ return $ MRWS.evalRWS evl (Context {maxReductions = maxRed, program = p, superTerms = []}) 0)
{-
    mapExceptionalT (\evl ->
        MW.WriterT $ return $
        (\(a,s,w) -> trace (show s) (a,w)) $
        MRWS.runRWS evl (maxRed,p) 0)
-}


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
        y <- localSuperTerm i [] [xs] $ full x
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
        Node f args -> fmap (Node f) $ mapArgs f full args
        Number _ _ -> return x'
        StringLiteral _ _ -> return x'

-- | evaluate until root symbol is constructor.
top :: Term -> Evaluator Term
top t = ( lift $ tell . (:[]) . Term . TermFocus t =<< asks superTerms ) >> case t of
    Number {} -> return t
    StringLiteral {} -> return t
    Node f xs ->
        if Term.isConstructor f
          then return t
          else eval f xs  >>=  top

mapArgs :: Identifier -> (Term -> Evaluator Term) -> [Term] -> Evaluator [Term]
mapArgs i f =
    let go _ [] = return []
        go done (x:xs) = do
            y <- localSuperTerm i done xs $ f x
            fmap (y:) $ go (y:done) xs
    in  go []

localSuperTerm ::
    (Monad m, Monoid w) =>
    Identifier ->
    [Term] ->
    [Term] ->
    ExceptionalT e (MRWS.RWST Context w s m) b ->
    ExceptionalT e (MRWS.RWST Context w s m) b
localSuperTerm i done xs =
    mapExceptionalT
        (MRWS.local (\ctx -> ctx{superTerms =
            TermFocus.Node i (TermFocus.List done xs) :
            superTerms ctx}))

-- | do one reduction step at the root
eval ::
    Identifier -> [Term] -> Evaluator Term
eval i xs
  | name i `elem` [ "compare", "<", "-", "+", "*", "div", "mod" ] = do
      ys <- mapArgs i top xs
      lift $ tell $ [ Source $ Step { target = i } ]
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

eval g ys = do
    funcs <- lift $ asks ( Program.functions . program )
    case M.lookup g funcs of
        Nothing ->
            exception (range g) $
            unwords [ "unknown function", show $ Node g ys ]
        Just rules ->
            evalDecls g rules ys


evalDecls ::
    Identifier -> [ Rule.Rule ] -> [Term] -> Evaluator Term
evalDecls g =
    foldr
        (\(Rule.Rule f xs rhs) go ys -> do
            lift $ tell [ Source $ AttemptRule f ]
            (m, ys') <- matchExpandList M.empty g [] xs ys
            case m of
                Nothing -> go ys'
                Just (substitions, additionalArgs) -> do
                    conss <- lift $ asks ( Program.constructors . program )
                    lift $ tell $ map Source $
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
                         ( m, ys' ) <- matchExpandList M.empty g [] xs ys
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
    Identifier ->
    [Term] ->
    [Term] ->
    [Term] ->
    Evaluator (Maybe (M.Map Identifier Term, [Term]), [Term])
matchExpandList s _ _ [] ys = return ( Just (s,ys), ys )
matchExpandList s i done (x:xs) (y:ys) = do
    (m, y') <- localSuperTerm i done ys $ matchExpand x y
    fmap (mapSnd (y':)) $
        case m of
            Nothing -> return ( Nothing, ys )
            Just s' -> do
                s'' <-
                    case MW.runWriter $ Trav.sequenceA $
                         M.unionWithKey (\var t _ -> MW.tell [var] >> t)
                             (fmap return s) (fmap return s') of
                        (un, []) -> return un
                        (_, vars) -> exception (termRange y') $
                            "variables bound more than once in pattern: " ++
                            intercalate ", " (map name vars)
                matchExpandList s'' i (y':done) xs ys
matchExpandList _ _ _ (x:_) _ =
    exception (termRange x) "too few arguments"

apply :: M.Map Identifier Term -> Term -> Evaluator Term
apply m t = checkMaxReductions (termRange t) >> case t of
    Node f xs -> do
        ys <- mapM ( apply m ) xs
        case M.lookup f m of
            Nothing -> return $ Node f ys
            Just t' -> appendArguments t' ys
    _ -> return t

checkMaxReductions :: Range -> Evaluator ()
checkMaxReductions rng = do
    maxCount <- lift $ asks maxReductions
    count <- lift get
    assertT (rng, "number of reductions exceeds limit " ++ show maxCount) $
        count < maxCount
    lift $ put $ succ count
