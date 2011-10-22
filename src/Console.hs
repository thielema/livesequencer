-- module Console where

import Term
import Rewrite
import Event
import Program ( Program (..), chase )

import qualified Option
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.Trans.Writer ( runWriter )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forM_ )

import Prelude hiding ( log )


-- | read rules files, start expansion of "main"
main :: IO ()
main = do
    opt <- Option.get
    p <- Program.chase (Option.importPaths opt) $ Option.moduleName opt
    withSequencer "Rewrite-Sequencer" $ \sq -> do
        startQueue sq
        MS.evalStateT ( execute p sq ( read "main" ) ) 0


execute ::
    Program ->
    Sequencer SndSeq.DuplexMode ->
    Term ->
    MS.StateT Time IO ()
execute p sq =
    let go t = do
            let (s, log) = runWriter $ force_head p t
            liftIO $ forM_ log print
            liftIO $ print s
            case s of
                Node i [] | name i == "[]" -> return ()
                Node i [x, xs] | name i == ":" -> do
                    play_event x sq
                    go xs
    in  go
