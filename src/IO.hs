module IO where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.PrettyPrint.HughesPJ

class Input a where input :: Parsec.Parser a
class Output a where output :: a -> Doc

parsec_reader ::
    (Input a) =>
    t -> String -> [(a, String)]
parsec_reader _p s =
    case Parsec.parse ( do x <- input ; t <- Parsec.getInput ; return (x,t) ) "" s of
      Left _err -> []
      Right (x,t) -> [(x,t)]
