module Common where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import Control.Monad (liftM2, )

import qualified System.IO as IO


data Sequencer mode =
   Sequencer (SndSeq.T mode) Port.T


sendEvent ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> Event.Data -> IO ()
sendEvent (Sequencer h p) ev = do
   c <- Client.getId h
   _ <-
      Event.outputDirect h $
      Event.simple (Addr.Cons c p) $ ev
   return ()


getWaitingEvents ::
   (SndSeq.AllowInput mode) =>
   Sequencer mode -> IO [Event.T]
getWaitingEvents (Sequencer h _) =
   let loop =
          AlsaExc.catch
             (liftM2 (:) (Event.input h) loop)
             (const $ return [])
   in  loop


withSequencer ::
   (SndSeq.OpenMode mode) =>
   String -> (Sequencer mode -> IO ()) -> IO ()
withSequencer name act =
   flip AlsaExc.catch
      (\e -> IO.hPutStrLn IO.stderr $ "alsa_exception: " ++ AlsaExc.show e) $ do
   SndSeq.with SndSeq.defaultName SndSeq.Nonblock $ \h -> do
   Client.setName h name
   Port.withSimple h "inout"
      (Port.caps [Port.capRead, Port.capSubsRead,
                  Port.capWrite, Port.capSubsWrite]) Port.typeApplication $ \ port -> do
   act $ Sequencer h port


-- | for compatibility with GHC-6.12, is in Control.Monad since GHC-7
void :: Functor f => f a -> f ()
void = fmap (const ())
