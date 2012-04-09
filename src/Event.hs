module Event where

import Term ( Term(Number, StringLiteral), termRange )
import ALSA ( Sequencer(handle, queue, privatePort), sendEvent )
import qualified Term
import qualified ALSA
import qualified Time
import qualified Exception
import qualified Log

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as SeqEvent
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Exception.Synchronous ( ExceptionalT, throwT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad ( when, forever )
import Control.Functor.HT ( void )

import Data.Monoid ( mempty, mappend )

import qualified System.Process as Proc
import qualified System.Exit as Exit
import qualified System.IO.Strict as StrictIO
import qualified System.IO as IO

import qualified Data.Accessor.Monad.Trans.State as AccM
import qualified Data.Accessor.Basic as Acc
import Data.Accessor.Basic ((^.), )

import qualified Data.Sequence as Seq
import Data.Maybe ( isJust )

import qualified Control.Concurrent.Split.Chan as Chan
import Control.Concurrent ( forkIO )



type Time = Time.Nanoseconds Integer

data WaitMode = RealTime | SlowMotion (Time.Milliseconds Integer) | SingleStep
    deriving (Eq, Show)

data WaitResult =
         ModeChange WaitMode | ReachedTime SeqEvent.TimeStamp | NextStep
    deriving (Show)


termException ::
    (Monad m) =>
    Term -> String -> ExceptionalT Exception.Message m a
termException s msg =
    throwT $
    Exception.Message Exception.Term
        (termRange s) (msg ++ " " ++ show s)


runIO :: (MonadIO m) => IO a -> ExceptionalT Exception.Message m a
runIO action = MT.lift $ liftIO action


checkRange ::
    (Bounded a, Monad m) =>
    String -> (Int -> a) -> (a -> Int) ->
    a -> a ->
    Term ->
    ExceptionalT Exception.Message m a
checkRange typ fromInt toInt minb maxb =
    Exception.lift .
    Exception.checkRange Exception.Term typ fromInt toInt minb maxb

checkRangeAuto ::
    (Bounded a, Monad m) =>
    String -> (Int -> a) -> (a -> Int) ->
    Term ->
    ExceptionalT Exception.Message m a
checkRangeAuto typ fromInt0 toInt0 =
    Exception.lift .
    Exception.checkRangeAuto Exception.Term typ fromInt0 toInt0


data State =
    State {
        stateWaitMode_ :: WaitMode,
        stateWaiting_ :: Bool,
        stateTime_ :: Time,
        stateRecentTimes_ :: Seq.Seq Time
    }

stateWaitMode :: Acc.T State WaitMode
stateWaitMode = Acc.fromSetGet (\x s -> s{stateWaitMode_ = x}) stateWaitMode_

stateWaiting :: Acc.T State Bool
stateWaiting = Acc.fromSetGet (\x s -> s{stateWaiting_ = x}) stateWaiting_

stateTime :: Acc.T State Time
stateTime = Acc.fromSetGet (\x s -> s{stateTime_ = x}) stateTime_

stateRecentTimes :: Acc.T State (Seq.Seq Time)
stateRecentTimes = Acc.fromSetGet (\x s -> s{stateRecentTimes_ = x}) stateRecentTimes_

initState :: State
initState = State Event.RealTime False mempty Seq.empty

runState :: (Monad m) => MS.StateT State m a -> m a
runState = flip MS.evalStateT Event.initState


play ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    (Exception.Message -> IO ()) ->
    Term ->
    ExceptionalT Exception.Message (MS.StateT State IO) (Maybe Time)
play sq throwAsync x = case Term.viewNode x of
    Just ("Wait", [Number _ n]) -> do
        when (n<0) $ termException x $
            "pause of negative duration: " ++ show n
        MT.lift $ AccM.set stateWaiting True
        return $ Just $ Time.milliseconds n

    Just ( "Say", [StringLiteral rng arg] ) ->
            MT.lift $ (AccM.set stateWaiting False >>) $ liftIO $ do
        let cmd = unwords
                      [ "echo", show arg, "|", "festival", "--tts" ]
        Log.put cmd
        void $ forkIO $ do
            (inp,_out,err,pid) <-
                Proc.runInteractiveProcess
                    "festival" [ "--tts" ] Nothing Nothing
            void $ forkIO (IO.hPutStr inp arg >> IO.hClose inp)
            errText <- StrictIO.hGetContents err
            exitCode <- Proc.waitForProcess pid
            case exitCode of
                Exit.ExitSuccess ->
                    when (not (null errText)) $
                    throwAsync $
                    Exception.Message Exception.Term rng ("warning: " ++ errText)
                Exit.ExitFailure _ ->
                    throwAsync $
                    Exception.Message Exception.Term rng errText

        return Nothing

    Just ("Event", [event]) -> case Term.viewNode event of
        Just ("Channel", [chann, body]) -> do
            chan <-
                checkRange "channel" id id
                    0 (Seq.length (ALSA.ports sq) * 16 - 1) chann
            let (p, c) = divMod chan 16
            processChannelMsg sq (Seq.index (ALSA.ports sq) p, CM.toChannel c) body
        _ -> processChannelMsg sq (ALSA.publicPort sq, CM.toChannel 0) event
           -- termException x "Event must contain Channel, but not "
    _ -> termException x "can only process Wait or Event, but not "

processChannelMsg ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    (Port.T, CM.Channel) -> Term ->
    ExceptionalT Exception.Message (MS.StateT State IO) (Maybe Time)
processChannelMsg sq chanPort@(port, chan) body = do
    MT.lift $ AccM.set stateWaiting False
    let checkVelocity =
            checkRange "velocity" CM.toVelocity CM.fromVelocity
                (CM.toVelocity 1) (CM.toVelocity 127)
    case Term.viewNode body of
        Just ("On", [pn, vn]) -> do
            p <- checkRangeAuto "pitch" CM.toPitch CM.fromPitch pn
            v <- checkVelocity vn
            runIO $ sendNote sq SeqEvent.NoteOn chanPort p v
        Just ("Off", [pn, vn]) -> do
            p <- checkRangeAuto "pitch" CM.toPitch CM.fromPitch pn
            v <- checkVelocity vn
            runIO $ sendNote sq SeqEvent.NoteOff chanPort p v
        Just ("PgmChange", [pn]) -> do
            p <- checkRangeAuto "program" CM.toProgram CM.fromProgram pn
            runIO $
                sendEvent sq port $ SeqEvent.CtrlEv SeqEvent.PgmChange $
                MidiAlsa.programChangeEvent chan p
        Just ("Controller", [ccn, vn]) -> do
            cc <- checkRangeAuto "controller" CM.toController CM.fromController ccn
            v <- checkRange "controller value" id id 0 127 vn
            runIO $
                sendEvent sq port $ SeqEvent.CtrlEv SeqEvent.Controller $
                MidiAlsa.controllerEvent chan cc (fromIntegral v)
        _ -> termException body "invalid channel event: "
    return Nothing


wait ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    Chan.Out WaitResult ->
    Maybe Time ->
    MS.StateT State IO ()
wait sq waitChan mdur = do
    let loop target = do
           liftIO $ Log.put $ "Chan.read waitChan"
           ev <- liftIO $ Chan.read waitChan
           liftIO $ Log.put $ "read from waitChan: " ++ show ev
           case ev of
               ModeChange newMode -> do
                   oldMode <- AccM.get stateWaitMode
                   if newMode /= oldMode
                     then do
                         AccM.set stateWaitMode newMode
                         (cont,newTarget) <- prepare sq mdur
                         when cont $ loop newTarget
                     else loop target
               ReachedTime stamp ->
                   case stamp of
                       SeqEvent.RealTime rt ->
                           let reached =
                                   Time.nanoseconds $ RealTime.toInteger rt
                           in  if Just reached == target
                                 then AccM.set stateTime reached
                                 else loop target
                       _ -> loop target
               NextStep ->
                   when (isJust target) $ loop target

    (cont,targetTime) <- prepare sq mdur
    when cont $ loop targetTime


prepare ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode -> Maybe Time ->
    MS.StateT State IO (Bool, Maybe Time)
prepare sq mt = do
    liftIO $ Log.put $ "prepare waiting for " ++ show mt
    (State waitMode _ currentTime _) <- MS.get
    let sendEchoCont t = do
            sendEcho sq t
            return (True, Just t)
    case waitMode of
        RealTime -> do
            case mt of
                Nothing -> return (False, Nothing)
                Just dur -> sendEchoCont $ mappend currentTime dur
        SlowMotion dur ->
            sendEchoCont $ mappend currentTime $ Time.up $ Time.up dur
        SingleStep ->
            return (True, Nothing)


sendEcho ::
    (MonadIO io, SndSeq.AllowOutput mode) =>
    Sequencer mode -> Time ->
    io ()
sendEcho sq (Time.Time t) = do
    c <- liftIO $ Client.getId (handle sq)

    {-
    liftIO $ Log.put . ("wait, send echo for " ++) . show =<< MS.get
    -}
    let dest =
            Addr.Cons {
               Addr.client = c,
               Addr.port = privatePort sq
            }

    liftIO $ Log.put $ "send echo message to " ++ show dest
    liftIO $ void $ SeqEvent.output (handle sq) $
       (SeqEvent.simple
          (Addr.Cons c Port.unknown)
          (SeqEvent.CustomEv SeqEvent.Echo (SeqEvent.Custom 0 0 0)))
          { SeqEvent.queue = queue sq
          , SeqEvent.timestamp =
                SeqEvent.RealTime $ RealTime.fromInteger t
          , SeqEvent.dest = dest
          }

    liftIO $ void $ SeqEvent.drainOutput (handle sq)


{-
We cannot concurrently wait for different kinds of events.
Thus we run one thread that listens to all incoming events
and distributes them to who they might concern.
-}
listen ::
    (SndSeq.AllowInput mode) =>
    Sequencer mode ->
    (VM.Pitch -> IO ()) ->
    Chan.In WaitResult -> IO ()
listen sq noteInput waitChan = do
    Log.put "listen to ALSA port"
    c <- Client.getId (handle sq)

    let dest =
            Addr.Cons {
               Addr.client = c,
               Addr.port = privatePort sq
            }

    forever $ do
        Log.put "wait, wait for echo"
        ev <- SeqEvent.input (handle sq)
        Log.put $ "wait, get message " ++ show ev
        case SeqEvent.body ev of
            SeqEvent.NoteEv SeqEvent.NoteOn note ->
                noteInput $ note ^. MidiAlsa.notePitch
            SeqEvent.CustomEv SeqEvent.Echo _ ->
                when (dest == SeqEvent.dest ev) $ do
                    Log.put "write waitChan"
                    Chan.write waitChan $ ReachedTime $ SeqEvent.timestamp ev
            _ -> return ()


sendNote ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    SeqEvent.NoteEv ->
    (Port.T, CM.Channel) ->
    CM.Pitch ->
    CM.Velocity ->
    IO ()
sendNote sq onoff (port,chan) pitch velocity =
    sendEvent sq port $
    SeqEvent.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
