module IO where

import Text.Parsec
import Text.PrettyPrint.HughesPJ

class Input a where input :: Parsec String () a
class Output a where output :: a -> Doc

parsec_reader ::
    (Input a) =>
    t -> String -> [(a, String)]
parsec_reader _p s =
    case parse ( do x <- input ; t <- getInput ; return (x,t) ) "" s of
      Left _err -> []
      Right (x,t) -> [(x,t)]
