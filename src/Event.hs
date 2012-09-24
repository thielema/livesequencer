{- |
The tricky part of the event scheduling is how to achieve precise timing.

We want to achieve:

* Precisely timed sending of MIDI events.

* Immediate start of music when you start the interpreter.

* Immediate stop of music when you stop the interpreter.

We achieve precise timing by sending the events with a fixed delay
that the user can set with the 'latency' command line option.
That is, if latency is 0.1s,
then we tell ALSA at time point t to send an event at t+0.1s.
This way we make ALSA responsible for precise timing,
and it actually makes a good job.
In order to prevent cumulation of rounding errors
we maintain the ideal time in the 'stateTime' field of our 'State' record.
This time should always be a little bit smaller than the time in the ALSA queue.

In total, for every element in the MIDI stream we send two ALSA events:

* If the current event is @Wait@ and we are in real-time mode,
  then we send an Echo event with evaluateId to ourselves
  with a delay according to the @Wait@ value.

* If the current event is @Event@,
  then we send the according MIDI event via ALSA with a delay of latency
  with respect to the current ideal time.

* In any case we send a delayed Echo event with visualizeId to ourselves.
  This Echo makes sure that the display is updated
  when ALSA ships the event and
  not immediately after the computation of the event.

The 'wait' function is responsible for waiting between events
depending on the execution mode.
Actually, the essence of execution modes is how we wait.
In real-time mode it waits according to @Wait@ events.
In slow-motion mode it waits a fixed duration between events.
In single-step mode it waits for @Next step@ user commands.
The function also reacts to execution mode changes within a waiting period.
Actually, changes to execution mode can only happen during waiting.

The very tricky part is how to react immediately
to start and stop of the interpreter.
Of course, if the user requests interpreter start
we cannot send an event immediately
because we have to compute one first.
However, we pretend that the first event is sent immediately
and send following events according to the ideal start time.
The interpreter has to compute many events at once
until it is @latency@ ahead of the current ALSA queue time.
If the interpreter is paused
then events for the duration of @latency@ will remain in the queue.
If the user switches back from pause to realtime execution
then the stored events shall be shipped regularly.
However, if the user requests the next step in single step mode,
then these events must be shipped at once,
since we do not want to simply drop them.
If we would drop them we would risk hanging tones.
We achieve the shipping at once
by moving the ALSA queue time by @latency@ forward.

We use the trick of increasing the ALSA queue time at several places.
We use it whenever it is necessary to react immediately to a user request.
In order to avoid adding up multiple latencies
we always add the latency to the ideal time,
not to the current ALSA queue time.
We also have to take care of order of ALSA MIDI events.
Although stopping the interpreter
shall immediately send an AllNotesOff,
we must schedule the AllNotesOff event with a latency
and increment ALSA queue time
in order to assert correct ordering with previous and following MIDI events.
In contrast to that we must send some Queue control commands
like QueueContinue immediately,
because the queue might be stopped and
in this state scheduled events may not be processed.
-}
module Event where

import Term ( Term(Number, StringLiteral), termRange )
import ALSA ( Sequencer(handle), Time )
import qualified Term
import qualified ALSA
import qualified Time
import qualified Exception
import qualified Log

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import qualified Sound.MIDI.ALSA as MidiAlsa
import qualified Sound.MIDI.MachineControl as MMC

import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as ATime
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as SeqEvent
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Exception.Asynchronous as ExcA
import Control.Monad.Exception.Synchronous ( ExceptionalT, throwT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad ( when, forever )
import Control.Functor.HT ( void )
import Data.Foldable ( forM_ )

import Data.Monoid ( mempty, mappend )

import qualified System.Process as Proc
import qualified System.Exit as Exit
import qualified System.IO.Strict as StrictIO
import qualified System.IO as IO
import Data.IORef ( newIORef, readIORef, modifyIORef )

import qualified Data.Accessor.Monad.Trans.State as AccM
import qualified Data.Accessor.Basic as Acc
import Data.Accessor.Basic ((^.), )

import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import Data.Maybe ( isJust )
import Data.Bool.HT ( if' )

import qualified Control.Concurrent.Split.Chan as Chan
import Control.Concurrent ( forkIO )


data WaitMode =
         RealTime | SlowMotion (Time.Milliseconds Integer) | SingleStep Continue
    deriving (Eq, Show)

data WaitResult =
         ModeChange WaitMode | ReachedTime Time | NextStep Continue |
         AlsaSend (MS.StateT State ALSA.Send ())

data Continue =
         NextElement | NextReduction | NextReductionShow
    deriving (Eq, Show)


singleStep :: WaitMode
singleStep = SingleStep NextElement


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


runSend ::
    Sequencer SndSeq.DuplexMode ->
    MS.StateT s ALSA.Send a ->
    MS.StateT s IO a
runSend sq = MS.mapStateT (ALSA.runSend sq)


play ::
    Sequencer SndSeq.DuplexMode ->
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
            Time.pause $ ALSA.latencyMicro sq
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
    Sequencer SndSeq.DuplexMode ->
    (Port.T, CM.Channel) -> Term ->
    ExceptionalT Exception.Message (MS.StateT State IO) (Maybe Time)
processChannelMsg sq chanPort@(port, chan) body = do
    MT.lift $ AccM.set stateWaiting False
    let checkVelocity =
            checkRange "velocity" CM.toVelocity CM.fromVelocity
                (CM.toVelocity 1) (CM.toVelocity 127)
        runSendE = MT.lift . runSend sq
    case Term.viewNode body of
        Just ("On", [pn, vn]) -> do
            p <- checkRangeAuto "pitch" CM.toPitch CM.fromPitch pn
            v <- checkVelocity vn
            runSendE $ sendNote SeqEvent.NoteOn chanPort p v
        Just ("Off", [pn, vn]) -> do
            p <- checkRangeAuto "pitch" CM.toPitch CM.fromPitch pn
            v <- checkVelocity vn
            runSendE $ sendNote SeqEvent.NoteOff chanPort p v
        Just ("PgmChange", [pn]) -> do
            p <- checkRangeAuto "program" CM.toProgram CM.fromProgram pn
            runSendE $
                sendEvent port $ SeqEvent.CtrlEv SeqEvent.PgmChange $
                MidiAlsa.programChangeEvent chan p
        Just ("Controller", [ccn, vn]) -> do
            cc <- checkRangeAuto "controller" CM.toController CM.fromController ccn
            v <- checkRange "controller value" id id 0 127 vn
            runSendE $
                sendEvent port $ SeqEvent.CtrlEv SeqEvent.Controller $
                MidiAlsa.controllerEvent chan cc (fromIntegral v)
        _ -> termException body "invalid channel event: "
    return Nothing


wait ::
    Sequencer SndSeq.DuplexMode ->
    Chan.Out WaitResult ->
    Maybe Time ->
    MS.StateT State IO ()
wait sq waitChan mdur = do
    let loop target = do
           liftIO $ Log.put $ "Chan.read waitChan"
           ev <- liftIO $ Chan.read waitChan
           -- liftIO $ Log.put $ "read from waitChan: " ++ show ev
           case ev of
               ModeChange newMode -> do
                   oldMode <- AccM.get stateWaitMode
                   if newMode /= oldMode
                     then do
                         AccM.set stateWaitMode newMode
                         (cont,newTarget) <- runSend sq $ prepare mdur
                         when cont $ loop newTarget
                     else loop target
               ReachedTime reached ->
                   {- check for equality only works
                      because we have not set TimeStamping -}
                   if Just reached == target
                     then AccM.set stateTime reached
                     else loop target
               NextStep cont -> do
                   AccM.set stateWaitMode $ SingleStep cont
                   runSend sq forwardStoppedQueue
                   when (isJust target) $ loop target
               AlsaSend send -> do
                   runSend sq send
                   loop target

    (cont,targetTime) <- runSend sq $ prepare mdur
    when cont $ loop targetTime


forwardStoppedQueue ::
    MS.StateT State ALSA.Send ()
forwardStoppedQueue = do
    sq <- ALSA.askSeq
    t <- fmap (mappend (ALSA.latencyNano sq)) $ AccM.get stateTime
    ALSA.forwardStoppedQueue t

forwardQuietContinueQueue ::
    MS.StateT State ALSA.Send ()
forwardQuietContinueQueue = do
    sq <- ALSA.askSeq
    t <- fmap (mappend (ALSA.latencyNano sq)) $ AccM.get stateTime
    ALSA.sendAllNotesOffLater t
    ALSA.forwardContinueQueue t

forwardStopQueue ::
    MS.StateT State ALSA.Send ()
forwardStopQueue = do
    sq <- ALSA.askSeq
    t <- fmap (mappend (ALSA.latencyNano sq)) $ AccM.get stateTime
    ALSA.sendAllNotesOffLater t
    ALSA.forwardStoppedQueue t


prepare ::
    Maybe Time ->
    MS.StateT State ALSA.Send (Bool, Maybe Time)
prepare mdur = do
    -- liftIO $ Log.put $ "prepare waiting for " ++ show mdur
    waitMode <- AccM.get stateWaitMode
    let sendEchoCont d = do
            t <- sendEcho evaluateId d
            return (True, Just t)
    case waitMode of
        RealTime -> do
            case mdur of
                Nothing -> return (False, Nothing)
                Just dur -> sendEchoCont dur
        SlowMotion dur -> sendEchoCont $ Time.up $ Time.up dur
        SingleStep _ -> return (True, Nothing)


newtype EchoId = EchoId SeqEvent.Tag
    deriving (Eq, Show)

evaluateId, visualizeId :: EchoId
evaluateId  = EchoId (SeqEvent.Tag 0)
visualizeId = EchoId (SeqEvent.Tag 1)


sendEcho ::
    EchoId -> Time ->
    MS.StateT State ALSA.Send Time
sendEcho (EchoId echoId) dur = do
    {-
    liftIO $ Log.put . ("wait, send echo for " ++) . show =<< MS.get
    -}
    dest <- ALSA.makeSend ALSA.privateAddress

    currentTime <- AccM.get stateTime
    let targetTime = mappend currentTime dur

    -- liftIO $ Log.put $ "send echo message to " ++ show dest
    ALSA.sendEventOnQueue $
        (SeqEvent.simple dest
            (SeqEvent.CustomEv SeqEvent.Echo
                (SeqEvent.customZero)))
           { SeqEvent.tag = echoId,
             SeqEvent.dest = dest,
             SeqEvent.time =
                 ALSA.realTimeStamp targetTime
           }

    return targetTime


{-
We cannot concurrently wait for different kinds of ALSA sequencer events.
Thus we run one thread that listens to all incoming ALSA sequencer events
and distributes them to who they might concern.
-}
listen ::
    (SndSeq.AllowInput mode) =>
    Sequencer mode ->
    (VM.Pitch -> IO ()) ->
    IO () ->
    Chan.In WaitResult -> IO ()
listen sq noteInput visualize waitChan = do
    Log.put "listen to ALSA port"

    dest <- ALSA.privateAddress sq
    recording <- newIORef True

    forever $ do
        Log.put "wait, wait for echo"
        ev <- SeqEvent.input (handle sq)
        Log.put $ "wait, get message " ++ show ev
        case SeqEvent.body ev of
            SeqEvent.NoteEv SeqEvent.NoteOn note ->
                readIORef recording
                    >>= flip when (noteInput $ note ^. MidiAlsa.notePitch)
            SeqEvent.ExtEv SeqEvent.SysEx msg ->
                {- FIXME: How to cope with the device id? -}
                case B.unpack msg of
                    0xF0 : 0x7F : 0x00 : 0x06 : cmds ->
                        forM_ (ExcA.result $ fst $
                               MMC.runParser MMC.getCommands cmds) $ \cmd ->
                            case cmd of
                                MMC.RecordStrobe -> modifyIORef recording not
                                _ -> return ()
                    _ -> return ()
            SeqEvent.CustomEv SeqEvent.Echo _cust ->
                when (dest == SeqEvent.dest ev) $
                    if' (EchoId (SeqEvent.tag ev) == evaluateId)
                        (case SeqEvent.time ev of
                            ATime.Cons ATime.Absolute (ATime.Real rt) -> do
                                Log.put "write waitChan"
                                Chan.write waitChan $ ReachedTime $ 
                                    Time.nanoseconds $ RealTime.toInteger rt
                            _ -> return ())
                        (do
                            Log.put "visualize"
                            visualize)
            _ -> return ()


sendNote ::
    SeqEvent.NoteEv ->
    (Port.T, CM.Channel) ->
    CM.Pitch ->
    CM.Velocity ->
    MS.StateT State ALSA.Send ()
sendNote onoff (port,chan) pitch velocity =
    sendEvent port $ SeqEvent.NoteEv onoff $
    MidiAlsa.noteEvent chan pitch velocity velocity 0

sendEvent ::
    Port.T -> SeqEvent.Data ->
    MS.StateT State ALSA.Send ()
sendEvent p ev = do
    sq <- ALSA.askSeq
    currentTime <- AccM.get stateTime
    ALSA.sendEventOnQueue $
        (SeqEvent.forSourcePort p ev) {
            SeqEvent.time =
                ALSA.realTimeStamp $
                mappend currentTime $ ALSA.latencyNano sq
        }
