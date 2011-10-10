-- module Console where

import IO
import Term
import Program
import Rewrite
import Event

import Common

import Text.Parsec
import System.Environment
import Control.Monad ( forM )

import Control.Concurrent

-- | read rules file, start expansion of "main"
main = do
    [ f ] <- getArgs
    s <- readFile f
    case parse input f s of
        Left err -> print err
        Right p -> withSequencer "Mode" $ execute p ( read "main" )
      
execute p t sq = do      
    let s = force_head p t
    print s
    case s of
        Node (Identifier "Nil") [] -> return ()
        Node (Identifier "Cons") [x, xs] -> do
          play_event x sq
          execute p xs sq
      