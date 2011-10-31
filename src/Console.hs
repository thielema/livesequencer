-- module Console where

import Term
import qualified Rewrite
import Event
import Program ( Program (..), chase )

import qualified Option
import qualified ALSA

import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.Exception.Synchronous
          ( Exceptional(Success, Exception), resolveT )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forM_ )

import qualified System.IO as IO
import qualified System.Exit as Exit

import Prelude hiding ( log )


-- | read rules files, start expansion of "main"
main :: IO ()
main = do
    opt <- Option.get
    p <-
        resolveT (\e -> IO.hPutStrLn IO.stderr (show e) >> Exit.exitFailure) $
        Program.chase (Option.importPaths opt) $ Option.moduleName opt
    ALSA.withSequencer "Rewrite-Sequencer" $ \sq -> do
        ALSA.parseAndConnect sq ( Option.connectTo opt )
        ALSA.startQueue sq
        MS.evalStateT ( execute p sq ( read "main" ) ) 0


execute ::
    Program ->
    ALSA.Sequencer SndSeq.DuplexMode ->
    Term ->
    MS.StateT Time IO ()
execute p sq =
    let go t = do
            let (ms, log) = Rewrite.runEval (Rewrite.force_head t) p
            liftIO $ forM_ log print
            liftIO $ print ms
            case ms of
                Exception (pos, msg) ->
                    liftIO $ IO.hPutStrLn IO.stderr $ show pos ++ " " ++ msg
                Success s ->
                    case s of
                        Node i [] | name i == "[]" -> return ()
                        Node i [x, xs] | name i == ":" -> do
                            liftIO . mapM_ print =<< play_event x sq
                            go xs
                        _ -> liftIO $ IO.hPutStrLn IO.stderr $
                             "do not know how to handle term\n" ++ show s
    in  go
