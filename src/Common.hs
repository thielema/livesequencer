module Common where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import qualified Sound.MIDI.Message.Channel.Mode as ModeMsg
import qualified Sound.MIDI.ALSA as MIDI

import qualified System.IO as IO

import Control.Monad ( (<=<) )


data Sequencer mode =
   Sequencer (SndSeq.T mode) Port.T Queue.T


type Time = Integer


sendEvent ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> Event.Data -> IO ()
sendEvent (Sequencer h p _) ev = do
   c <- Client.getId h
   void $
      Event.outputDirect h $
      Event.simple (Addr.Cons c p) ev
   return ()

startQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
startQueue (Sequencer h _ q) = do
   -- putStrLn "start queue"
   Queue.control h q Event.QueueStart 0 Nothing
   void $ Event.drainOutput h

stopQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
stopQueue sq@(Sequencer h _ q) = do
   -- putStrLn "stop queue"
   mapM_ (Event.output h) =<< allNotesOff sq
   Queue.control h q Event.QueueStop 0 Nothing
   void $ Event.drainOutput h

pauseQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
pauseQueue (Sequencer h _ q) = do
   -- putStrLn "pause queue"
   Queue.control h q Event.QueueStop 0 Nothing
   void $ Event.drainOutput h

continueQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
continueQueue (Sequencer h _ q) = do
   -- putStrLn "continue queue"
   Queue.control h q Event.QueueContinue 0 Nothing
   void $ Event.drainOutput h

quietContinueQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
quietContinueQueue sq@(Sequencer h _ q) = do
   -- putStrLn "continue queue"
   mapM_ (Event.output h) =<< allNotesOff sq
   Queue.control h q Event.QueueContinue 0 Nothing
   void $ Event.drainOutput h

allNotesOff ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO [Event.T]
allNotesOff (Sequencer h p _) = do
   c <- Client.getId h
   return $
      map (Event.simple (Addr.Cons c p) .
           Event.CtrlEv Event.Controller .
           flip MIDI.modeEvent ModeMsg.AllNotesOff)
         [minBound .. maxBound]

parseAndConnect ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> Maybe String -> IO ()
parseAndConnect (Sequencer h p _) =
   maybe (return ()) (SndSeq.connectTo h p <=< Addr.parse h)


withSequencer ::
   (SndSeq.OpenMode mode) =>
   String -> (Sequencer mode -> IO ()) -> IO ()
withSequencer name act =
   flip AlsaExc.catch
      (\e -> IO.hPutStrLn IO.stderr $ "alsa_exception: " ++ AlsaExc.show e) $ do
   SndSeq.with SndSeq.defaultName SndSeq.Block $ \h -> do
   Client.setName h name
   Port.withSimple h "inout"
      (Port.caps [Port.capRead, Port.capSubsRead,
                  Port.capWrite, Port.capSubsWrite]) Port.typeApplication $ \ port -> do
   Queue.with h $ \queue -> do
   act $ Sequencer h port queue


-- | for compatibility with GHC-6.12, is in Control.Monad since GHC-7
void :: Functor f => f a -> f ()
void = fmap (const ())
