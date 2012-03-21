-- module Console where

import Term
import Program ( Program )
import qualified Program
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
import Control.Monad ( when, forM_, (>=>) )
import Control.Functor.HT ( void )

import qualified System.IO as IO
import Option.Utility ( exitFailureMsg )

import Prelude hiding ( log )


-- | read rules files, start expansion of "main"
main :: IO ()
main = do
    opt <- Option.get
    when (null $ Option.moduleNames opt) $
        exitFailureMsg "no module specified"
    p <-
        resolveT (exitFailureMsg . Exception.multilineFromMessage) $
        Program.chaseMany
            (Option.importPaths opt) (Option.moduleNames opt) Program.empty
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
                Rewrite.runEval p (Rewrite.forceHead t)
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
