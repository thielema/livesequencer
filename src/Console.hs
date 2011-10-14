-- module Console where

import IO
import Term
import Rewrite
import Event
import Program ( Program )

import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import Text.Parsec
import System.Environment
import Control.Monad.Trans.Writer ( runWriter )
import Control.Monad ( forM, forM_ )

import Prelude hiding ( log )


-- | read rules files, start expansion of "main"
main :: IO ()
main = do
    fs <- getArgs
    ss <- forM fs readFile 
    let s = concat ss
    case parse input "fs" s of
        Left err -> print err
        Right p -> withSequencer "Rewrite-Sequencer" $ execute p ( read "main" )


execute ::
    Program ->
    Term ->
    Sequencer SndSeq.OutputMode ->
    IO ()
execute p t sq = do
    let (s, log) = runWriter $ force_head p t
    forM_ log print
    print s
    case s of
        Node i [] | name i == "Nil" -> return ()
        Node i [x, xs] | name i == "Cons" -> do
          play_event x sq
          execute p xs sq

