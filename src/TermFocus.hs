module TermFocus where

import qualified Term
import Term ( Term, Identifier )
import IO ( Output, output )

import Text.PrettyPrint.HughesPJ ( Doc, (<+>), fsep, parens, render, text )


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

mark :: String -> String
mark str = "{{" ++ str ++ "}}"

format :: TermFocus -> String
format = render . output

fromTerm :: Term -> TermFocus
fromTerm t = TermFocus t []
