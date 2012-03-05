-- module Console where

import Term
import Program ( Program (..), chase )
import qualified Event
import qualified Rewrite
import qualified Exception

import qualified Option
import qualified ALSA

import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.State as MS
import Control.Monad.Exception.Synchronous
          ( mapExceptionalT, resolveT, throwT )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( forM_, (>=>) )
import Control.Functor.HT ( void )

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
    ALSA.withSequencer opt $ \sq -> do
        waitChan <- newChan
        void $ forkIO $ Event.listen sq print waitChan
        ALSA.startQueue sq
        Event.runState $
            execute p sq waitChan mainName

writeExcMsg :: Exception.Message -> IO ()
writeExcMsg = putStrLn . Exception.statusFromMessage

execute ::
    Program ->
    ALSA.Sequencer SndSeq.DuplexMode ->
    Chan Event.WaitResult ->
    Term ->
    MS.StateT Event.State IO ()
execute p sq waitChan =
    let go t = do
            s <-
                mapExceptionalT
                    (MW.runWriterT >=> \(ms,log) ->
                        forM_ log (liftIO . print) >> return ms) $
                Rewrite.runEval p (Rewrite.force_head t)
            lift $ liftIO $ print s
            case Term.viewNode s of
                Just ("[]", []) -> return ()
                Just (":", [x, xs]) -> do
                    mdur <- lift $ resolveT
                        (liftIO . fmap (const Nothing) . writeExcMsg)
                        (Event.play sq writeExcMsg x)
                    lift $ Event.wait sq waitChan mdur
                    go xs
                _ -> throwT
                        (termRange s,
                         "do not know how to handle term\n" ++ show s)
    in  resolveT
            (\(pos, msg) ->
                liftIO $ IO.hPutStrLn IO.stderr $ show pos ++ " " ++ msg)
         . go
