module Event where

import Term
import ALSA ( Sequencer(handle, queue, privatePort), sendEvent )
import Utility ( void )
import qualified Exception
import qualified Log

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad ( when, forever )

import Data.Accessor.Basic ((^.), )

import Data.Bool.HT ( if' )

import Control.Concurrent.Chan
-- import Control.Concurrent ( threadDelay )


type Time = Integer


termException :: String -> Term -> Exception.Message
termException msg s =
    Exception.Message Exception.Term
        (termRange s) (msg ++ " " ++ show s)


runIO :: (MonadIO m) => IO () -> m [Exception.Message]
runIO action = liftIO action >> return []

{-
FIXME:
minBound for Velocity is zero.
This is not very helpful, because zero velocity is treated as NoteOff.
-}
withRangeCheck ::
    (Bounded a, Monad m) =>
    String -> (Int -> a) -> (a -> Int) ->
    Term ->
    (a -> m [Exception.Message]) -> m [Exception.Message]
withRangeCheck typ fromInt0 toInt0 (Number rng x) =
    let aux ::
            (Monad m) =>
            (Int -> a) -> (a -> Int) ->
            a -> a -> (a -> m [Exception.Message]) -> m [Exception.Message]
        aux fromInt toInt minb maxb f =
            if' (x < fromIntegral (toInt minb))
                (return [Exception.Message Exception.Term rng $
                         typ ++ " argument " ++ show x ++
                             " is less than minimum value " ++ show (toInt minb)]) $
            if' (fromIntegral (toInt maxb) < x)
                (return [Exception.Message Exception.Term rng $
                         typ ++ " argument " ++ show x ++
                              " is greater than maximum value " ++ show (toInt maxb)]) $
            f (fromInt $ fromInteger x)
    in  aux fromInt0 toInt0 minBound maxBound
withRangeCheck typ _ _ t =
    \ _f -> return $ [termException (typ ++ " argument is not a number") t]


newtype ControllerValue = ControllerValue {fromControllerValue :: Int}
    deriving (Eq, Ord, Show)

instance Bounded ControllerValue where
    minBound = ControllerValue 0
    maxBound = ControllerValue 127

play_event ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    Chan Event.TimeStamp ->
    Term ->
    MS.StateT Time IO [ Exception.Message ]
play_event sq waitChan x = case Term.viewNode x of
    Just ("Wait", [Number _ n]) ->
--        threadDelay (fromIntegral n * 1000)
        wait sq waitChan (10^(6::Int) * n)
        >>
        return []
    Just ("Event", [event]) -> case Term.viewNode event of
        Just ("Channel", [chann, body]) ->
            withRangeCheck "channel" CM.toChannel CM.fromChannel chann $ \chan ->
                case Term.viewNode body of
                    Just ("On", [pn, vn]) ->
                        withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
                        withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
                        runIO $
                        sendNote sq Event.NoteOn chan p v
                    Just ("Off", [pn, vn]) ->
                        withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
                        withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
                        runIO $
                        sendNote sq Event.NoteOff chan p v
                    Just ("PgmChange", [pn]) ->
                        withRangeCheck "program" CM.toProgram CM.fromProgram pn $ \p ->
                        runIO $
                        sendEvent sq $ Event.CtrlEv Event.PgmChange $
                            MidiAlsa.programChangeEvent chan p
                    Just ("Controller", [ccn, vn]) ->
                        withRangeCheck "controller" CM.toController CM.fromController ccn $ \cc ->
                        withRangeCheck "controller value" ControllerValue fromControllerValue vn $ \(ControllerValue v) ->
                        runIO $
                        sendEvent sq $ Event.CtrlEv Event.Controller $
                            MidiAlsa.controllerEvent chan cc (fromIntegral v)
                    _ -> return [ termException "unknown channel event" x ]
        _ -> return [ termException "Event must contain Channel, but not " x ]
    _ -> return [ termException "can only process Wait or Event, but not " x ]


wait ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode -> Chan Event.TimeStamp -> Time -> MS.StateT Time IO ()
wait sq waitChan t = do
    c <- liftIO $ Client.getId (handle sq)
    {-
    liftIO $ Log.put . ("wait, current time " ++) . show =<< MS.get
    -}
    MS.modify (t+)
    targetTime <- MS.get

    {-
    liftIO $ Log.put . ("wait, send echo for " ++) . show =<< MS.get
    -}
    let dest =
            Addr.Cons {
               Addr.client = c,
               Addr.port = privatePort sq
            }

    liftIO $ Log.put $ "send echo message to " ++ show dest
    void $ liftIO $ Event.output (handle sq) $
       (Event.simple
          (Addr.Cons c Port.unknown)
          (Event.CustomEv Event.Echo (Event.Custom 0 0 0)))
          { Event.queue = queue sq
          , Event.timestamp =
                Event.RealTime $ RealTime.fromInteger targetTime
          , Event.dest = dest
          }

    void $ liftIO $ Event.drainOutput (handle sq)

    let loop = do
           Log.put $ "readChan waitChan"
           time <- readChan waitChan
           Log.put $ "read from waitChan: " ++ show time
           when
               (case time of
                    Event.RealTime rt ->
                        RealTime.toInteger rt /= targetTime
                    _ -> True)
               loop
    liftIO $ loop


{-
We cannot concurrently wait for different kinds of events.
Thus we run one thread that listens to all incoming events
and distributes them to who they might concern.
-}
listen ::
    (SndSeq.AllowInput mode) =>
    Sequencer mode ->
    (VM.Pitch -> IO ()) ->
    Chan Event.TimeStamp -> IO ()
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
        ev <- Event.input (handle sq)
        Log.put $ "wait, get message " ++ show ev
        case Event.body ev of
            Event.NoteEv Event.NoteOn note ->
                noteInput $ note ^. MidiAlsa.notePitch
            Event.CustomEv Event.Echo _ ->
                when (dest == Event.dest ev) $ do
                    Log.put "write waitChan"
                    writeChan waitChan (Event.timestamp ev)
            _ -> return ()


sendNote ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    Event.NoteEv ->
    CM.Channel ->
    CM.Pitch ->
    CM.Velocity ->
    IO ()
sendNote sq onoff chan pitch velocity =
    sendEvent sq $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
