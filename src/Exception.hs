module Exception where

import qualified Term
import Term ( Range(Range) )

import qualified Control.Monad.Exception.Synchronous as Exc

import qualified Text.ParserCombinators.Parsec.Error as PErr
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import qualified Text.ParserCombinators.Parsec as Parsec

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
    head (lines descr) :
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


toParsec :: Message -> Parsec.Parser a
toParsec (Message _ rng msg) = do
    Parsec.setPosition $ Term.start rng
    fail msg

messageFromParserError :: PErr.ParseError -> Message
messageFromParserError err = Message
    Parse
    (let p = PErr.errorPos err
     in  Range p (Pos.updatePosChar p ' '))
    (removeLeadingNewline $
     PErr.showErrorMessages
         "or" "unknown parse error"
         "expecting" "unexpected" "end of input" $
     PErr.errorMessages err)

removeLeadingNewline :: String -> String
removeLeadingNewline ('\n':str) = str
removeLeadingNewline str = str


dummyRange :: String -> Range
dummyRange f =
    let pos = Pos.initialPos f
    in  Range pos pos



-- also available in explicit-exception>=0.1.7
switchT ::
    (Monad m) =>
    (e -> m b) -> (a -> m b) ->
    Exc.ExceptionalT e m a -> m b
switchT e s m = Exc.switch e s =<< Exc.runExceptionalT m

lift ::
    (Monad m) =>
    Exc.Exceptional e a -> Exc.ExceptionalT e m a
lift = Exc.ExceptionalT . return
