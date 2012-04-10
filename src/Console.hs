-- module Console where

import Term ( Term )
import Program ( Program )
import qualified Program
import qualified Time
import qualified Term
import qualified Event
import qualified Rewrite
import qualified Exception

import qualified Option
import qualified ALSA

import qualified Sound.ALSA.Sequencer as SndSeq
-- import qualified Sound.ALSA.Sequencer.Event as SeqEvent

import Control.Concurrent ( forkIO )
import qualified Control.Concurrent.Split.Chan as Chan

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.State as MS
import Control.Monad.Exception.Synchronous
          ( mapExceptionalT, resolveT, throwT )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( when, (>=>) )
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
        (waitIn,waitOut) <- Chan.new
        (visIn,visOut) <- Chan.new
        void $ forkIO $ Event.listen sq print (Chan.read visOut >>= print) waitIn
        ALSA.runSend sq ALSA.startQueue
        Event.runState $
            execute
                (Option.maxReductions $ Option.limits opt)
                p sq visIn waitOut Term.mainName

writeExcMsg :: Exception.Message -> IO ()
writeExcMsg = putStrLn . Exception.statusFromMessage

execute ::
    Rewrite.Count ->
    Program ->
    ALSA.Sequencer SndSeq.DuplexMode ->
    Chan.In Term ->
    Chan.Out Event.WaitResult ->
    Term ->
    MS.StateT Event.State IO ()
execute maxRed p sq visIn waitOut =
    let go t = do
            s <-
                mapExceptionalT
                    (MW.runWriterT >=> \(ms,_log) ->
                        {- liftIO (mapM_ print log) >> -} return ms) $
                Rewrite.runEval maxRed p (Rewrite.forceHead t)
            lift $ liftIO $ Chan.write visIn s
            lift $ void $ Event.runSend sq$
                Event.sendEcho Event.visualizeId $ ALSA.latencyNano sq
            case Term.viewNode s of
                Just (":", [x, xs]) -> do
                    mdur <- lift $ resolveT
                        (liftIO . fmap (const Nothing) . writeExcMsg)
                        (Event.play sq writeExcMsg x)
                    lift $ Event.wait sq waitOut mdur
                    go xs
                Just ("[]", []) ->
                    lift $ liftIO $
                    Time.pause $ ALSA.latencyMicro sq
                    {- says: operation not permitted
                    SeqEvent.syncOutputQueue (ALSA.handle sq)
                    -}
                _ -> throwT
                        (Term.termRange s,
                         "do not know how to handle term\n" ++ show s)
    in  resolveT
            (\(pos, msg) ->
                liftIO $ IO.hPutStrLn IO.stderr $ show pos ++ " " ++ msg)
         . go
