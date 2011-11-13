-- module Console where

import Term
import Program ( Program (..), chase )
import Utility ( void )
import qualified Event
import qualified Rewrite
import qualified Exception

import qualified Option
import qualified ALSA

import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan

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
        resolveT (\e ->
            IO.hPutStrLn IO.stderr (Exception.statusFromMessage e) >>
            Exit.exitFailure) $
        Program.chase (Option.importPaths opt) $ Option.moduleName opt
    ALSA.withSequencer "Rewrite-Sequencer" $ \sq -> do
        waitChan <- newChan
        void $ forkIO $ Event.listen sq print waitChan
        ALSA.parseAndConnect sq
            ( Option.connectFrom opt ) ( Option.connectTo opt )
        ALSA.startQueue sq
        flip MS.evalStateT (Event.RealTime, 0) $
            execute p sq waitChan ( read "main" )


execute ::
    Program ->
    ALSA.Sequencer SndSeq.DuplexMode ->
    Chan Event.WaitResult ->
    Term ->
    MS.StateT Event.State IO ()
execute p sq waitChan =
    let go t = do
            let (ms, log) = Rewrite.runEval (Rewrite.force_head t) p
            liftIO $ forM_ log print
            liftIO $ print ms
            case ms of
                Exception (pos, msg) ->
                    liftIO $ IO.hPutStrLn IO.stderr $ show pos ++ " " ++ msg
                Success s ->
                    case Term.viewNode s of
                        Just ("[]", []) -> return ()
                        Just (":", [x, xs]) -> do
                            resolveT
                                (liftIO . putStrLn . Exception.statusFromMessage)
                                (Event.play sq waitChan x)
                            go xs
                        _ -> liftIO $ IO.hPutStrLn IO.stderr $
                             "do not know how to handle term\n" ++ show s
    in  go
