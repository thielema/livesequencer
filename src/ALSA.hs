module ALSA where

import qualified Option

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

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Cont ( ContT(ContT), runContT, mapContT )
import Control.Monad ( (<=<) )
import Control.Functor.HT ( void )
import Data.Foldable ( Foldable, forM_ )


data Sequencer mode =
   Sequencer {
      handle :: SndSeq.T mode,
      publicPort, privatePort :: Port.T,
      queue :: Queue.T
   }


sendEvent ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> Event.Data -> IO ()
sendEvent sq ev = do
   c <- Client.getId (handle sq)
   void $
      Event.outputDirect (handle sq) $
      Event.simple (Addr.Cons c (publicPort sq)) ev

queueControl ::
   Sequencer mode -> Event.QueueEv -> IO ()
queueControl sq cmd =
   Queue.control (handle sq) (queue sq) cmd 0 Nothing

drainOutput ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
drainOutput sq =
   void $ Event.drainOutput (handle sq)

startQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
startQueue sq = do
   -- Log.put "start queue"
   queueControl sq Event.QueueStart
   drainOutput sq

stopQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
stopQueue sq = do
   -- Log.put "stop queue"
   mapM_ (Event.output (handle sq)) =<< allNotesOff sq
   queueControl sq Event.QueueStop
   drainOutput sq

pauseQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
pauseQueue sq = do
   -- Log.put "pause queue"
   queueControl sq Event.QueueStop
   drainOutput sq

continueQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
continueQueue sq = do
   -- Log.put "continue queue"
   queueControl sq Event.QueueContinue
   drainOutput sq

quietContinueQueue ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO ()
quietContinueQueue sq = do
   -- Log.put "continue queue"
   mapM_ (Event.output (handle sq)) =<< allNotesOff sq
   queueControl sq Event.QueueContinue
   drainOutput sq

allNotesOff ::
   (SndSeq.AllowOutput mode) =>
   Sequencer mode -> IO [Event.T]
allNotesOff sq = do
   c <- Client.getId (handle sq)
   return $
      map (Event.simple (Addr.Cons c (publicPort sq)) .
           Event.CtrlEv Event.Controller .
           flip MIDI.modeEvent ModeMsg.AllNotesOff)
         [minBound .. maxBound]

parseAndConnect ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode, Foldable f) =>
   Sequencer mode ->
   f String -> f String -> IO ()
parseAndConnect sq from to = do
   forM_ from
      (SndSeq.connectFrom (handle sq) (publicPort sq)
       <=<
       Addr.parse (handle sq))
   forM_ to
      (SndSeq.connectTo (handle sq) (publicPort sq)
       <=<
       Addr.parse (handle sq))


withSequencer ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   Option.Option -> (Sequencer mode -> IO ()) -> IO ()
withSequencer opt =
   runContT $
   mapContT (flip AlsaExc.catch
      (\e -> IO.hPutStrLn IO.stderr $ "alsa_exception: " ++ AlsaExc.show e)) $ do
   h <- ContT $ SndSeq.with SndSeq.defaultName SndSeq.Block
   liftIO $ Client.setName h $ Option.sequencerName opt
   public <- ContT $
      Port.withSimple h "inout"
         (Port.caps [Port.capRead, Port.capSubsRead,
                     Port.capWrite, Port.capSubsWrite])
         (Port.types [Port.typeMidiGeneric, Port.typeSoftware, 
                      Port.typeApplication])
   private <- ContT $
      Port.withSimple h "echo"
         (Port.caps [Port.capRead, Port.capWrite])
         (Port.types [Port.typeSpecific])
   q <- ContT $ Queue.with h
   let sq = Sequencer h public private q
   liftIO $ parseAndConnect sq
       ( Option.connectFrom opt ) ( Option.connectTo opt )
   return sq
