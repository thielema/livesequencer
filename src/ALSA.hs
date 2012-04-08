{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ALSA where

import qualified Option
import qualified Time
-- import qualified Log

import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as ATime
import qualified Sound.ALSA.Sequencer.Connect as Connect
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

import qualified Data.Sequence as Seq
import qualified Utility.NonEmptyList as NEList

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Cont ( ContT(ContT), runContT, mapContT )
import Control.Monad ( (<=<) )
import Control.Applicative ( Applicative )
import Control.Functor.HT ( void )
import qualified Data.Foldable as Fold
import Data.Foldable ( Foldable, forM_, foldMap )
import Data.Monoid ( mappend )


data Sequencer mode =
   Sequencer {
      handle :: SndSeq.T mode,
      publicPort, privatePort :: Port.T,
      ports :: Seq.Seq Port.T,
      queue :: Queue.T,
      latencyNano  :: Time.Nanoseconds  Integer,
      latencyMicro :: Time.Microseconds Int
   }


privateAddress ::
   (SndSeq.OpenMode mode) =>
   Sequencer mode -> IO Addr.T
privateAddress sq = do
   c <- Client.getId (handle sq)

   return $
      Addr.Cons {
         Addr.client = c,
         Addr.port = privatePort sq
      }


type Time = Time.Nanoseconds Integer

realTime :: Time -> RealTime.T
realTime (Time.Time time) =
   RealTime.fromInteger time

realTimeStamp :: Time -> ATime.T
realTimeStamp =
   ATime.consAbs . ATime.Real . realTime


newtype Send a = Send (MR.ReaderT (Sequencer SndSeq.DuplexMode) IO a)
   deriving (Functor, Applicative, Monad)

runSend :: Sequencer SndSeq.DuplexMode -> Send a -> IO a
runSend sq (Send m) = do
   a <- MR.runReaderT m sq
   void $ Event.drainOutput (handle sq)
   return a


class Monad send => SendClass send where
   liftSend :: Send a -> send a

instance SendClass Send where
   liftSend = id

instance SendClass send => SendClass (MS.StateT s send) where
   liftSend = MT.lift . liftSend

makeSend ::
   SendClass send =>
   (Sequencer SndSeq.DuplexMode -> IO a) -> send a
makeSend act =
   liftSend $ Send $ MR.ReaderT act

askSeq ::
   SendClass send =>
   send (Sequencer SndSeq.DuplexMode)
askSeq = makeSend return


sendEvent :: SendClass send => Event.T -> send ()
sendEvent ev = makeSend $ \sq ->
   void $ Event.output (handle sq) ev

sendEventOnQueue :: SendClass send => Event.T -> send ()
sendEventOnQueue ev = do
   sq <- askSeq
   sendEvent $ ev { Event.queue = queue sq }

queueControl ::
   SendClass send =>
   Event.QueueEv -> Maybe Event.T -> send ()
queueControl cmd proto =
   makeSend $ \sq -> Queue.control (handle sq) (queue sq) cmd proto

startQueue :: SendClass send => send ()
startQueue = do
   -- Log.put "start queue"
   queueControl Event.QueueStart Nothing

stopQueue :: SendClass send => send ()
stopQueue = do
   -- Log.put "stop queue"
   mapM_ sendEvent =<< allNotesOff
   queueControl Event.QueueStop Nothing

stopQueueDelayed :: SendClass send => Time -> send Time
stopQueueDelayed t = do
   sq <- askSeq
   let targetTime = mappend t $ latencyNano sq
   -- Log.put $ "stop queue delayed from " ++ show t ++ " to " ++ show targetTime
   let stamp ev =
           ev{Event.queue = queue sq,
              Event.time = realTimeStamp targetTime}
   mapM_ (sendEvent . stamp) =<< allNotesOff
   queueControl Event.QueueStop $ Just $ stamp $
       Event.simple Addr.unknown $ Event.EmptyEv Event.None
   return targetTime

pauseQueue ::SendClass send => send ()
pauseQueue = do
   -- Log.put "pause queue"
   queueControl Event.QueueStop Nothing

continueQueue :: SendClass send => send ()
continueQueue = do
   -- Log.put "continue queue"
   queueControl Event.QueueContinue Nothing

quietContinueQueue :: SendClass send => send ()
quietContinueQueue = do
   -- Log.put "continue queue"
   mapM_ sendEvent =<< allNotesOff
   queueControl Event.QueueContinue Nothing

allNotesOff :: SendClass send => send [Event.T]
allNotesOff =
   makeSend $ \sq -> return $ do
      port <- Fold.toList $ ports sq
      chan <- [minBound .. maxBound]
      return $
         Event.forSourcePort port $
         Event.CtrlEv Event.Controller $
         MIDI.modeEvent chan ModeMsg.AllNotesOff



forwardQueue :: SendClass send => Time -> send ()
forwardQueue t = do
   -- Log.put "forward queue"
   queueControl (Event.QueueSetPosTime (realTime t)) Nothing

forwardStoppedQueue :: SendClass send => Time -> send ()
forwardStoppedQueue t = do
   -- Log.put "forward stopped queue"
   queueControl Event.QueueContinue Nothing
   queueControl (Event.QueueSetPosTime (realTime t)) Nothing
   queueControl Event.QueueStop Nothing


parseAndConnect ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   SndSeq.T mode ->
   Option.Port -> ContT () IO Port.T
parseAndConnect h (Option.Port name from to) = do
   let caps =
          mappend
             (foldMap (const $ Port.caps [Port.capWrite, Port.capSubsWrite]) from)
             (foldMap (const $ Port.caps [Port.capRead,  Port.capSubsRead]) to)
   p <- ContT $
      Port.withSimple h name caps
         (Port.types [Port.typeMidiGeneric, Port.typeSoftware, 
                      Port.typeApplication])
   liftIO $ forM_ from $ mapM_ (Connect.createFrom h p <=< Addr.parse h)
   liftIO $ forM_ to   $ mapM_ (Connect.createTo   h p <=< Addr.parse h)
   return p


withSequencer ::
   (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
   Option.Option -> (Sequencer mode -> IO ()) -> IO ()
withSequencer opt@(Option.Option { Option.connect = NEList.Cons firstPort otherPorts } ) =
   runContT $
   mapContT (flip AlsaExc.catch
      (\e -> IO.hPutStrLn IO.stderr $ "alsa_exception: " ++ AlsaExc.show e)) $ do
   h <- ContT $ SndSeq.with SndSeq.defaultName SndSeq.Block
   liftIO $ Client.setName h $ Option.sequencerName opt
   public <- parseAndConnect h firstPort
   ps <-
      fmap ((public Seq.<|) . Seq.fromList) $
      mapM (parseAndConnect h) otherPorts
   private <- ContT $
      Port.withSimple h "echo"
         (Port.caps [Port.capRead, Port.capWrite])
         (Port.types [Port.typeSpecific])
   q <- ContT $ Queue.with h
   return $
      Sequencer {
          handle = h,
          publicPort = public,
          privatePort = private,
          ports = ps,
          queue = q,
          latencyNano = fmap round $ Time.seconds (Option.latency opt),
          latencyMicro = fmap round $ Time.seconds (Option.latency opt)
      }
