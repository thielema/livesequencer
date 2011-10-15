module Common where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import qualified System.IO as IO


data Sequencer mode =
   Sequencer (SndSeq.T mode) Port.T Queue.T


type Time = Integer


sendEvent ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> Event.Data -> IO ()
sendEvent (Sequencer h p _) ev = do
   c <- Client.getId h
   _ <-
      Event.outputDirect h $
      Event.simple (Addr.Cons c p) $ ev
   return ()

startQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
startQueue (Sequencer h _ q) = do
   putStrLn "start queue"
   Queue.control h q Event.QueueStart 0 Nothing
   void $ Event.drainOutput h

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
