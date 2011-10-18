-- module Console where

import IO
import Term
import Rewrite
import Event
import Program ( Program (..), chase )

import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import Text.Parsec
import System.Environment

import qualified Control.Monad.Trans.State as MS
import Control.Monad.Trans.Writer ( runWriter )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forM, forM_ )

import Prelude hiding ( log )


-- | read rules files, start expansion of "main"
main :: IO ()
main = do
    [ f ] <- getArgs
    p <- Program.chase [ ".", "data" ] $ read f
    withSequencer "Rewrite-Sequencer" $ \sq -> do
                startQueue sq
                MS.evalStateT ( execute p ( read "main" ) sq ) 0


execute ::
    Program ->
    Term ->
    Sequencer SndSeq.DuplexMode ->
    MS.StateT Time IO ()
execute p t sq = do
    let (s, log) = runWriter $ force_head p t
    liftIO $ forM_ log print
    liftIO $ print s
    case s of
        Node i [] | name i == "Nil" -> return ()
        Node i [x, xs] | name i == "Cons" -> do
          play_event x sq
          execute p xs sq

