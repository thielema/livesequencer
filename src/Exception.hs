module Exception where

import Term ( Range(Range) )

import qualified Text.ParserCombinators.Parsec.Pos as Pos

import qualified Data.List as List


data Message = Message Type Range String
--    deriving (Show)

data Type = Parse | Term | InOut
    deriving (Show, Eq, Ord, Enum)


lineFromMessage :: Message -> [String]
lineFromMessage (Message typ (Range pos _) descr) =
    Pos.sourceName pos :
    show (Pos.sourceLine pos) : show (Pos.sourceColumn pos) :
    stringFromType typ :
    flattenMultiline descr :
    []

statusFromMessage :: Message -> String
statusFromMessage (Message typ (Range pos _) descr) =
    stringFromType typ ++ " - " ++
    formatPos typ pos ++ " - " ++
    flattenMultiline descr

multilineFromMessage :: Message -> String
multilineFromMessage (Message typ (Range pos _) descr) =
    stringFromType typ ++ " - " ++
    formatPos typ pos ++ "\n" ++
    descr

formatPos :: Type -> Pos.SourcePos -> String
formatPos typ pos =
    Pos.sourceName pos ++
    (case typ of
        InOut -> ""
        _ ->
            ':' : show (Pos.sourceLine pos) ++
            ':' : show (Pos.sourceColumn pos))

stringFromType :: Type -> String
stringFromType typ =
    case typ of
        Parse -> "parse error"
        Term  -> "term error"
        InOut -> "in/out error"

flattenMultiline :: String -> String
flattenMultiline =
    List.intercalate "; " . lines
