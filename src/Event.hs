module Event where

import Term
import ALSA ( Sequencer(handle, queue, privatePort), sendEvent )
import Utility ( void )
-- import qualified Log

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad ( when )

import Data.Bool.HT ( if' )

-- import Control.Concurrent ( threadDelay )


type Time = Integer


termException :: String -> Term -> (Range, String)
termException msg s =
    (termRange s, msg ++ " " ++ show s)


runIO :: (MonadIO m) => IO () -> m [(Range, String)]
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
    (a -> m [(Range, String)]) -> m [(Range, String)]
withRangeCheck typ fromInt0 toInt0 (Number rng x) =
    let aux ::
            (Monad m) =>
            (Int -> a) -> (a -> Int) ->
            a -> a -> (a -> m [(Range, String)]) -> m [(Range, String)]
        aux fromInt toInt minb maxb f =
            if' (x < fromIntegral (toInt minb))
                (return [(rng, typ ++ " argument " ++ show x ++
                              " is less than minimum value " ++ show (toInt minb))]) $
            if' (fromIntegral (toInt maxb) < x)
                (return [(rng, typ ++ " argument " ++ show x ++
                              " is greater than maximum value " ++ show (toInt maxb))]) $
            f (fromInt $ fromInteger x)
    in  aux fromInt0 toInt0 minBound maxBound
withRangeCheck typ _ _ t =
    \ _f -> return [(termRange t, typ ++ " argument is not a number")]


newtype ControllerValue = ControllerValue {fromControllerValue :: Int}
    deriving (Eq, Ord, Show)

instance Bounded ControllerValue where
    minBound = ControllerValue 0
    maxBound = ControllerValue 127

play_event ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Term ->
    Sequencer mode ->
    MS.StateT Time IO [ (Range, String) ]
play_event x sq = case x of
    Node i [Number _ n] | name i == "Wait" ->
--        threadDelay (fromIntegral n * 1000)
        wait sq (10^(6::Int) * n)
        >>
        return []
    Node ie [event] | name ie == "Event" -> case event of
        Node ic [chann, body] | name ic == "Channel" ->
            withRangeCheck "channel" CM.toChannel CM.fromChannel chann $ \chan ->
                case body of
                    Node i [pn, vn] | name i == "On" ->
                        withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
                        withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
                        runIO $
                        sendNote sq Event.NoteOn chan p v
                    Node i [pn, vn] | name i == "Off" ->
                        withRangeCheck "pitch" CM.toPitch CM.fromPitch pn $ \p ->
                        withRangeCheck "velocity" CM.toVelocity CM.fromVelocity vn $ \v ->
                        runIO $
                        sendNote sq Event.NoteOff chan p v
                    Node i [pn] | name i == "PgmChange" ->
                        withRangeCheck "program" CM.toProgram CM.fromProgram pn $ \p ->
                        runIO $
                        sendEvent sq $ Event.CtrlEv Event.PgmChange $
                            MidiAlsa.programChangeEvent chan p
                    Node i [ccn, vn] | name i == "Controller" ->
                        withRangeCheck "controller" CM.toController CM.fromController ccn $ \cc ->
                        withRangeCheck "controller value" ControllerValue fromControllerValue vn $ \(ControllerValue v) ->
                        runIO $
                        sendEvent sq $ Event.CtrlEv Event.Controller $
                            MidiAlsa.controllerEvent chan cc (fromIntegral v)
                    _ -> return [ termException "unknown channel event" x ]
        _ -> return [ termException "Event must contain Channel, but not " x ]
    _ -> return [ termException "can only process Wait or Event, but not " x ]


wait ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Sequencer mode -> Time -> MS.StateT Time IO ()
wait sq t = do
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
    {-
    liftIO $ Log.put $ "send echo message to " ++ show dest
    -}
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
            -- Log.put "wait, wait for echo"
            ev <- Event.input (handle sq)
            -- Log.put $ "wait, get message " ++ show ev
            let myEcho =
                   case Event.body ev of
                      Event.CustomEv Event.Echo _ ->
                         dest == Event.dest ev
                      _ -> False
            when (not myEcho) loop
    liftIO loop


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
