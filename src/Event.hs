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

import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as ATime
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

import Control.Concurrent.Chan ( Chan, readChan, writeChan )
import Control.Concurrent ( forkIO )

import Data.Word ( Word32 )
import Data.Bool.HT ( if' )


data WaitMode = RealTime | SlowMotion (Time.Milliseconds Integer) | SingleStep
    deriving (Eq, Show)

data WaitResult =
         ModeChange WaitMode | ReachedTime Time | NextStep |
         AlsaSend (MS.StateT State ALSA.Send ())


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
    Chan WaitResult ->
    Maybe Time ->
    MS.StateT State IO ()
wait sq waitChan mdur = do
    let loop target = do
           liftIO $ Log.put $ "readChan waitChan"
           ev <- liftIO $ readChan waitChan
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
               NextStep -> do
                   runSend sq forwardStoppedQueue
                   when (isJust target) $ loop target
               AlsaSend send -> do
                   runSend sq send
                   loop target

    (cont,targetTime) <- runSend sq $ prepare mdur
    when cont $ loop targetTime


forwardQueue ::
    MS.StateT State ALSA.Send ()
forwardQueue = do
    sq <- ALSA.askSeq
    ALSA.forwardQueue . mappend (ALSA.latencyNano sq)
        =<< AccM.get stateTime

forwardStoppedQueue ::
    MS.StateT State ALSA.Send ()
forwardStoppedQueue = do
    sq <- ALSA.askSeq
    ALSA.forwardStoppedQueue . mappend (ALSA.latencyNano sq)
        =<< AccM.get stateTime

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
        SingleStep -> return (True, Nothing)


newtype EchoId = EchoId Word32
    deriving (Eq, Show)

evaluateId, visualizeId :: EchoId
evaluateId  = EchoId 0
visualizeId = EchoId 1

echoIdFromCustom :: SeqEvent.Custom -> EchoId
echoIdFromCustom (SeqEvent.Custom echoWord _ _) =
    EchoId echoWord


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
                (SeqEvent.Custom echoId 0 0)))
           { SeqEvent.dest = dest,
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
    Chan WaitResult -> IO ()
listen sq noteInput visualize waitChan = do
    Log.put "listen to ALSA port"

    dest <- ALSA.privateAddress sq

    forever $ do
        Log.put "wait, wait for echo"
        ev <- SeqEvent.input (handle sq)
        Log.put $ "wait, get message " ++ show ev
        case SeqEvent.body ev of
            SeqEvent.NoteEv SeqEvent.NoteOn note ->
                noteInput $ note ^. MidiAlsa.notePitch
            SeqEvent.CustomEv SeqEvent.Echo cust ->
                when (dest == SeqEvent.dest ev) $
                    if' (echoIdFromCustom cust == evaluateId)
                        (case SeqEvent.time ev of
                            ATime.Cons ATime.Absolute (ATime.Real rt) -> do
                                Log.put "write waitChan"
                                writeChan waitChan $ ReachedTime $ 
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
