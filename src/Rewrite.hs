module Rewrite where

import Term
import Rule
import Program

import Control.Monad ( forM )
import Control.Monad.Trans.Writer ( Writer, tell )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Parsec.Pos as Pos
import Text.Parsec.Pos ( SourcePos )

import qualified Data.List as List
import qualified Data.Char as Char
import Data.Maybe ( maybeToList )

data Message = Step { target :: Identifier
                    , rule :: Maybe Identifier -- ^ Nothing for builtins
                    }
    deriving Show
             
target_position = position . target             
rule_position = fmap position . rule

-- | force head of stream:
-- evaluate until we have Cons or Nil at root,
-- then evaluate first argument of Cons fully.
force_head :: Program -> Term -> Writer [ Message ] Term
force_head p t = do
    t' <- top p t
    case t' of
      Node i [ x, xs ] | name i == "Cons"-> do
        y <- full p x
        return $ Node i [ y, xs ]
      Node i [] | name i == "Nil" ->
        return $ Node i []
      _ -> error $ "force_head: missing case for " ++ show t

-- | force full evaluation
-- (result has only constructors and numbers)
full :: Program -> Term -> Writer [ Message ] Term
full p x = do
    x' <- top p x
    case x' of
        Node f args -> do
            args' <- forM args $ full p
            return $ Node f args'
        Number n -> do
            return $ Number n

-- | evaluate until root symbol is constructor.
-- TODO: need to add tracing here
top :: Program -> Term -> Writer [ Message ] Term
top p t = case t of
    Number {} ->
        return t
    Node f _xs ->
        if isConstructor f then return t
        else do
            e <- eval p (rules p) t
            top p e

eval :: Program -> [ Rule ] -> Term -> Writer [ Message ] Term
eval p _ _t @ ( Node i xs )
  | name i `elem` [ "compare", "less", "minus", "plus", "times" ] = do
      ys <- forM xs $ full p -- these operations are strict
      tell $ [ Step { target = i, rule = Nothing } ]
      return $ case ( name i, ys ) of
           -- FIXME: handling of positions is dubious
           ( "less", [ Number a, Number b] ) ->
               Node ( Identifier { name = show (a < b), position = position i } ) []
           ( "compare", [ Number a, Number b] ) ->
               Node ( Identifier { name = show (compare a b), position = position i } ) []
           ( "minus", [ Number a, Number b] ) -> Number $ a - b
           ( "plus", [ Number a, Number b] ) -> Number $ a + b
           ( "times", [ Number a, Number b] ) -> Number $ a * b

eval _p [] t = error $ unwords [ "eval", show t ]
eval p (r : rs) t = do
  let Node f xs = lhs r ; Node g ys = t
  if f == g then do
        (m, ys') <- match_expand_list p xs ys
        let t' = Node g ys'
        case m of
               Nothing -> eval p rs t'
               Just sub -> do
                   tell [ Step { target =  g
                                 , rule = Just $ f } ]
                   return $ apply sub ( rhs r )
      else eval p rs t

-- | check whether term matches pattern.
-- do some reductions if they are necessary to decide about the match.
-- return the reduced term in the second result component.
match_expand :: Program -> Term -> Term
             -> Writer [ Message ] ( Maybe (M.Map Identifier Term) , Term )
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
    Writer [Message] (Maybe (M.Map Identifier Term), [Term])
match_expand_list _p [] [] = return ( return $ M.empty, [] )
match_expand_list p (x:xs) (y:ys) = do
    (m, y') <- match_expand p x y
    case m of
            Nothing -> return ( m, y' : ys )
            Just s  -> do
              (n, ys') <- match_expand_list p xs ys
              case n of
                  Nothing -> return ( n, y' : ys' )
                  Just s' -> return ( return $ M.unionWith (error "match_expand_list: non-linear pattern") s s'
                             , y' : ys' )

apply :: M.Map Identifier Term -> Term -> Term
apply m t = case t of
  Node f xs -> case M.lookup f m of
      Nothing -> Node f ( map ( apply m ) xs )
      Just t' -> t'
  _ -> t


sourcePosFromMessages ::
    [Message] -> M.Map FilePath (S.Set (Pos.Line,Pos.Column))
sourcePosFromMessages ms = M.fromListWith S.union $ do
    m <- ms
    pos <- [ target_position m ] ++ maybeToList ( rule_position m )
    return (Pos.sourceName pos,
                   S.singleton (Pos.sourceLine pos, Pos.sourceColumn pos))

highlight ::
    (S.Set (Pos.Line,Pos.Column)) -> String -> String
highlight poss =
    let countBase = 1
        diffs xs = zipWith (-) xs (countBase:xs)
        swap (x,y) = (y,x)
        linePoss =
            fmap (diffs . S.toAscList) $
            M.fromListWith S.union $ map (\(l,c) -> (l, S.singleton c)) $ S.toList poss
    in  unlines .
        zipWith (\ln str ->
            case M.lookup ln linePoss of
                Nothing -> str
                Just ds ->
                    (\css -> case css of
                        [] -> error "highlight: chunk list is always non-empty"
                        c:cs -> concat $ c : map highlightTokenHash cs) $
                    (\(rest, chunks) -> chunks++[rest]) $
                    List.mapAccumL
                        (\suffix d -> swap $ splitAt d suffix) str ds)
            [countBase ..] .
        lines

highlightTokenHash, highlightTokenCaps, highlightTokenBraces ::
    String -> String
highlightTokenHash = ('#':)
highlightTokenCaps =
    (\(ident,suffix) -> map Char.toUpper ident ++ suffix) .
    break Char.isSpace
highlightTokenBraces =
    (\(ident,suffix) -> '{' : ident ++ '}' : suffix) .
    break Char.isSpace
