module Event where

import Term
import Common (sendEvent, Sequencer )

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Concurrent ( threadDelay )


play_event ::
    Term ->
    Sequencer SndSeq.OutputMode ->
    IO ()
play_event x sq = case x of
    Node i [Number n] | name i == "Wait" ->
        threadDelay (fromIntegral n * 1000)
    Node ie [event] | name ie == "Event" -> case event of
        Node ic [Number c, body] | name ic == "Channel" ->
            let chan = ChannelMsg.toChannel $ fromIntegral c
            in  case body of
                    Node i [Number p, Number v] | name i == "On" ->
                        sendNote sq Event.NoteOn chan
                            (ChannelMsg.toPitch $ fromIntegral p)
                            (ChannelMsg.toVelocity $ fromIntegral v)
                    Node i [Number p, Number v] | name i == "Off" ->
                        sendNote sq Event.NoteOff chan
                            (ChannelMsg.toPitch $ fromIntegral p)
                            (ChannelMsg.toVelocity $ fromIntegral v)
                    Node i [Number p] | name i == "PgmChange" ->
                        sendEvent sq $ Event.CtrlEv Event.PgmChange $
                            MidiAlsa.programChangeEvent chan
                                (ChannelMsg.toProgram $ fromIntegral p)
                    _ -> putStrLn $ "Event.play_event: unknown channel event " ++ show x
        _ -> putStrLn $ "Event.play_event: Event must contain Channel, but not " ++ show x
    _ -> putStrLn $ "Event.play_event: can only process Wait or Event, but not " ++ show x

sendNote :: Sequencer SndSeq.OutputMode
         -> Event.NoteEv
            -> ChannelMsg.Channel -> ChannelMsg.Pitch -> ChannelMsg.Velocity
            -> IO ()
sendNote h onoff chan pitch velocity = do
  sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
