module Event where

import Term
import ALSA ( Sequencer(handle, queue, privatePort), sendEvent )
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

import Data.Monoid ( mappend )

import qualified System.Process as Proc
import qualified System.Exit as Exit
import qualified System.IO.Strict as StrictIO
import qualified System.IO as IO

import qualified Data.Accessor.Monad.Trans.State as AccM
import qualified Data.Accessor.Tuple as AccTuple
import Data.Accessor.Basic ((^.), )

import Data.Maybe ( isJust )
import Data.Bool.HT ( if' )

import Control.Concurrent.Chan ( Chan, readChan, writeChan )
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

{-
FIXME:
minBound for Velocity is zero.
This is not very helpful, because zero velocity is treated as NoteOff.
-}
withRangeCheck ::
    (Bounded a, Monad m) =>
    String -> (Int -> a) -> (a -> Int) ->
    Term ->
    (a -> ExceptionalT Exception.Message m b) ->
    ExceptionalT Exception.Message m b
withRangeCheck typ fromInt0 toInt0 (Number rng x) =
    let aux ::
            (Monad m) =>
            (Int -> a) -> (a -> Int) ->
            a -> a ->
            (a -> ExceptionalT Exception.Message m b) ->
            ExceptionalT Exception.Message m b
        aux fromInt toInt minb maxb f =
            if' (x < fromIntegral (toInt minb))
                (throwT $ Exception.Message Exception.Term rng $
                    typ ++ " argument " ++ show x ++
                        " is less than minimum value " ++ show (toInt minb)) $
            if' (fromIntegral (toInt maxb) < x)
                (throwT $ Exception.Message Exception.Term rng $
                         typ ++ " argument " ++ show x ++
                              " is greater than maximum value " ++ show (toInt maxb)) $
            f (fromInt $ fromInteger x)
    in  aux fromInt0 toInt0 minBound maxBound
withRangeCheck typ _ _ t =
    \ _f ->
        throwT $
        Exception.Message Exception.Term
            (termRange t) (typ ++ " argument is not a number")


newtype ControllerValue = ControllerValue {fromControllerValue :: Int}
    deriving (Eq, Ord, Show)

instance Bounded ControllerValue where
    minBound = ControllerValue 0
    maxBound = ControllerValue 127

type State = (WaitMode, Time)

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
        return $ Just $ Time.milliseconds n

    Just ( "Say", [String_Literal rng arg] ) -> runIO $ do
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
        Just ("Channel", [chann, body]) ->
            withRangeCheck "channel" CM.toChannel CM.fromChannel chann $ \chan ->
                processChannelMsg sq chan body
        _ -> processChannelMsg sq (CM.toChannel 0) event
           -- termException x "Event must contain Channel, but not "
    _ -> termException x "can only process Wait or Event, but not "

processChannelMsg ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    CM.Channel -> Term ->
    ExceptionalT Exception.Message (MS.StateT State IO) (Maybe Time)
processChannelMsg sq chan body = do
    case Term.viewNode body of
        Just ("On", [pn, vn]) ->
            withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
            withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
            runIO $
            sendNote sq SeqEvent.NoteOn chan p v
        Just ("Off", [pn, vn]) ->
            withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
            withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
            runIO $
            sendNote sq SeqEvent.NoteOff chan p v
        Just ("PgmChange", [pn]) ->
            withRangeCheck "program" CM.toProgram CM.fromProgram pn $ \p ->
            runIO $
            sendEvent sq $ SeqEvent.CtrlEv SeqEvent.PgmChange $
                MidiAlsa.programChangeEvent chan p
        Just ("Controller", [ccn, vn]) ->
            withRangeCheck "controller" CM.toController CM.fromController ccn $ \cc ->
            withRangeCheck "controller value" ControllerValue fromControllerValue vn $ \(ControllerValue v) ->
            runIO $
            sendEvent sq $ SeqEvent.CtrlEv SeqEvent.Controller $
                MidiAlsa.controllerEvent chan cc (fromIntegral v)
        _ -> termException body "invalid channel event: "
    return Nothing


wait ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    Chan WaitResult ->
    Maybe Time ->
    MS.StateT State IO ()
wait sq waitChan mdur = do
    let loop target = do
           liftIO $ Log.put $ "readChan waitChan"
           ev <- liftIO $ readChan waitChan
           liftIO $ Log.put $ "read from waitChan: " ++ show ev
           case ev of
               ModeChange newMode -> do
                   oldMode <- MS.gets fst
                   if newMode /= oldMode
                     then do
                         AccM.set AccTuple.first newMode
                         (cont,newTarget) <- prepare sq mdur
                         when cont $ loop newTarget
                     else loop target
               ReachedTime stamp ->
                   case stamp of
                       SeqEvent.RealTime rt ->
                           let reached =
                                   Time.nanoseconds $ RealTime.toInteger rt
                           in  if Just reached == target
                                 then AccM.set AccTuple.second reached
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
    (waitMode,currentTime) <- MS.get
    case waitMode of
        RealTime -> do
            case mt of
                Nothing -> return (False, Nothing)
                Just dur -> do
                    let t = mappend currentTime dur
                    sendEcho sq t
                    return (True, Just t)
        SlowMotion dur -> do
            let t = mappend currentTime $ Time.up $ Time.up dur
            sendEcho sq t
            return (True, Just t)
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
    Chan WaitResult -> IO ()
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
                    writeChan waitChan $ ReachedTime $ SeqEvent.timestamp ev
            _ -> return ()


sendNote ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    SeqEvent.NoteEv ->
    CM.Channel ->
    CM.Pitch ->
    CM.Velocity ->
    IO ()
sendNote sq onoff chan pitch velocity =
    sendEvent sq $
    SeqEvent.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
