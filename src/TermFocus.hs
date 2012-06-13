module TermFocus where

import qualified Term
import Term ( Term, Identifier )
import IO ( Output, output )

import Text.PrettyPrint.HughesPJ ( Doc, (<+>), fsep, parens, render, text )

import Data.Tuple.HT ( mapPair )
import Data.List.HT ( tails )
import Data.List ( isPrefixOf )


data TermFocus =
        TermFocus { subTerm :: Term, superTerms :: [ SuperTerm ] }
    deriving (Show)

data SuperTerm = Node Identifier ( List Term )
    deriving (Show)

data List a = List [a] [a]
    deriving (Show)

instance Output TermFocus where
    output tf =
        foldl (flip outputSuperTerm) (outputSubTerm (subTerm tf)) $
        superTerms tf

outputSuperTerm :: SuperTerm -> Doc -> Doc
outputSuperTerm (Node nm (List leftArgs rightArgs)) focus =
    output nm <+>
    fsep ( map Term.protected ( reverse leftArgs ) ++
           parens focus : map Term.protected rightArgs )

outputSubTerm :: Term -> Doc
outputSubTerm t = case t of
    Term.Number _ n -> text $ mark $ show n
    Term.StringLiteral _ s -> text $ mark $ show s
    Term.Node f args ->
        ( text $ mark $ Term.name f ) <+> fsep ( map Term.protected args )

markStart, markStop :: String
markStart = "{{"
markStop  = "}}"

mark :: String -> String
mark str = markStart ++ str ++ markStop

format :: TermFocus -> ((Int, Int), String)
format t =
    let s = render $ output t
    in  case splitAtInfix markStart s of
            (_, Nothing) -> ((0,0), s)
            (prefix, Just suffix0) ->
                let n = length prefix
                in  mapPair ((\j -> (n, n+j)), (prefix ++)) $
                    case splitAtInfix markStop suffix0 of
                        (_, Nothing) ->
                            (length suffix0, suffix0)
                        (marked, Just suffix) ->
                            (length marked, marked ++ suffix)

splitAtInfix :: Eq a => [a] -> [a] -> ([a], Maybe [a])
splitAtInfix infx str =
    mapPair
        (map head,
         \suffix ->
            case suffix of
                [] -> Nothing
                xs:_ -> Just $ drop (length infx) xs) $
    break (isPrefixOf infx) $ init $ tails str

fromTerm :: Term -> TermFocus
fromTerm t = TermFocus t []
