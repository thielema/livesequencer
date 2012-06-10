module TermFocus where

import Term ( Term, Identifier )

data TermFocus = Node Identifier ( List Term )
    deriving (Show)

data List a = List [a] [a]
    deriving (Show)
