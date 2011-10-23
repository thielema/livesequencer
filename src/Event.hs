module Event where

import qualified Rewrite
import Term
import Common ( Sequencer(Sequencer), sendEvent, Time, void )

import qualified Text.ParserCombinators.Parsec.Pos as Pos

import qualified Sound.MIDI.Message.Channel as ChannelMsg
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

-- import Control.Concurrent ( threadDelay )


termException :: String -> Term -> Rewrite.Message
termException msg s =
    case s of
        Node i _ ->
            Rewrite.Exception (Term.start i) Rewrite.TermException $
            msg ++ " " ++ show s
        Number n ->
            Rewrite.Exception  (Pos.initialPos "") Rewrite.TermException $
            "I do not know how to handle number literal " ++ show n


runIO :: (MonadIO m) => IO () -> m [Rewrite.Message]
runIO action = liftIO action >> return []

play_event ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Term ->
    Sequencer mode ->
    MS.StateT Time IO [ Rewrite.Message ]
play_event x sq = case x of
    Node i [Number n] | name i == "Wait" ->
--        threadDelay (fromIntegral n * 1000)
        wait sq (10^(6::Int) * n)
        >>
        return []
    Node ie [event] | name ie == "Event" -> case event of
        Node ic [Number c, body] | name ic == "Channel" ->
            let chan = ChannelMsg.toChannel $ fromIntegral c
            in  case body of
                    Node i [Number p, Number v] | name i == "On" ->
                        runIO $
                        sendNote sq Event.NoteOn chan
                            (ChannelMsg.toPitch $ fromIntegral p)
                            (ChannelMsg.toVelocity $ fromIntegral v)
                    Node i [Number p, Number v] | name i == "Off" ->
                        runIO $
                        sendNote sq Event.NoteOff chan
                            (ChannelMsg.toPitch $ fromIntegral p)
                            (ChannelMsg.toVelocity $ fromIntegral v)
                    Node i [Number p] | name i == "PgmChange" ->
                        runIO $
                        sendEvent sq $ Event.CtrlEv Event.PgmChange $
                            MidiAlsa.programChangeEvent chan
                                (ChannelMsg.toProgram $ fromIntegral p)
                    _ -> return [ termException "unknown channel event" x ]
        _ -> return [ termException "Event must contain Channel, but not " x ]
    _ -> return [ termException "can only process Wait or Event, but not " x ]


wait ::
    (SndSeq.AllowInput mode, SndSeq.AllowOutput mode) =>
    Sequencer mode -> Time -> MS.StateT Time IO ()
wait (Sequencer h p q) t = do
    c <- liftIO $ Client.getId h
    {-
    liftIO $ putStr "wait, current time "
    liftIO . print =<< MS.get
    -}
    MS.modify (t+)
    targetTime <- MS.get

    {-
    liftIO $ putStr "wait, send echo for "
    liftIO . print =<< MS.get
    -}
    void $ liftIO $ Event.output h $
       (Event.simple
          (Addr.Cons c Port.unknown)
          (Event.CustomEv Event.Echo (Event.Custom 0 0 0)))
          { Event.queue = q
          , Event.timestamp =
               Event.RealTime $ RealTime.fromInteger targetTime
          , Event.dest = Addr.Cons {
               Addr.client = c,
               Addr.port = p
            }
          }

    void $ liftIO $ Event.drainOutput h

    let loop = do
            -- putStrLn "wait, wait for echo"
            ev <- Event.input h
            -- putStr "wait, get message " >> print ev
            let myEcho =
                   case Event.body ev of
                      Event.CustomEv Event.Echo _ ->
                         c == Addr.client (Event.source ev)
                      _ -> False
            when (not myEcho) loop
    liftIO loop


sendNote ::
    (SndSeq.AllowOutput mode) =>
    Sequencer mode ->
    Event.NoteEv ->
    ChannelMsg.Channel ->
    ChannelMsg.Pitch ->
    ChannelMsg.Velocity ->
    IO ()
sendNote h onoff chan pitch velocity =
    sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
