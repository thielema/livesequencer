module Lilypond where

import qualified Text.ParserCombinators.Parsec as P

import Control.Monad (liftM2, )
import Control.Applicative ((<$))


{- |
Convert a monophonic melody in absolute pitch Lilypond notation
to live-sequencer notation.

E.g.

> toNotes "a'4 b,8 c4"
-}
toNotes :: String -> String
toNotes =
   either (error . show) id .
   P.parse parseNotes "input string" .
   filter (flip notElem "~[]")

parseNotes :: P.Parser String
parseNotes =
   parseOptional $
   P.many parseWhiteSpace +|+
      let go =
             parseOptional $
                parseNote
                +|+
                (parseOptional $
                 P.many1 parseWhiteSpace +|+ go)
      in  go

infixr 5 +|+

(+|+) :: Monad m => m [a] -> m [a] -> m [a]
(+|+) = liftM2 (++)

parseWhiteSpace :: P.Parser Char
parseWhiteSpace = P.oneOf " \t\n"

parseOptional :: P.Parser String -> P.Parser String
parseOptional p =
   p P.<|> return "[]"

parseNote :: P.Parser String
parseNote = do
   name <- P.many1 P.letter
   addDur <-
      case name of
         "r" -> return ("rest " ++)
         _ -> do
            octave <-
               (fmap length $ P.many1 $ P.char '\'')
               P.<|>
               (fmap (negate . length) $ P.many $ P.char ',')
            return $ \dur ->
               "note " ++ dur ++
               " (" ++ name ++ " " ++ show (octave+3) ++ ")"
   dur <-
      P.try ("s" <$ P.string "16") P.<|>
      ("e" <$ P.string "8") P.<|>
      ("q" <$ P.string "4") P.<|>
      ("h" <$ P.string "2") P.<|>
      ("w" <$ P.string "1")
   dotted <- P.optionMaybe (P.char '.')
   return $
      (addDur $ maybe id (++) ("d" <$ dotted) (dur++"n"))
      ++
      " ++"
