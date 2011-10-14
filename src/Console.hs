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
import Control.Monad.Writer
import Control.Concurrent

-- | read rules files, start expansion of "main"
main = do
    fs <- getArgs
    ss <- forM fs readFile 
    let s = concat ss
    case parse input "fs" s of
        Left err -> print err
        Right p -> withSequencer "Rewrite-Sequencer" $ execute p ( read "main" )

execute p t sq = do
    let (s, log) = runWriter $ force_head p t
    forM log print
    print s
    case s of
        Node i [] | name i == "Nil" -> return ()
        Node i [x, xs] | name i == "Cons" -> do
          play_event x sq
          execute p xs sq

