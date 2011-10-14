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

-- | read rules files, start expansion of "main"
main = do
    fs <- getArgs
    ss <- forM fs readFile 
    let s = concat ss
    case parse input "fs" s of
        Left err -> print err
        Right p -> withSequencer "Rewrite-Sequencer" $ execute p ( read "main" )

execute p t sq = do
    let s = force_head p t
    print s
    case s of
        Node (Identifier "Nil") [] -> return ()
        Node (Identifier "Cons") [x, xs] -> do
          play_event x sq
          execute p xs sq

