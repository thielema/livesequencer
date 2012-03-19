module IO where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.PrettyPrint.HughesPJ ( Doc )

import Control.Monad ( liftM2 )


class Input a where input :: Parsec.Parser a
class Output a where output :: a -> Doc

parsecReader ::
    (Input a) =>
    t -> String -> [(a, String)]
parsecReader _p s =
    case Parsec.parse ( liftM2 (,) input Parsec.getInput ) "" s of
      Left _err -> []
      Right (x,t) -> [(x,t)]
