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
            Node i [Number c, Number p, Number v] | name i == "On" ->
                sendNote sq Event.NoteOn (ChannelMsg.toChannel $ fromIntegral c)
                          (ChannelMsg.toPitch $ fromIntegral p)
                                       (ChannelMsg.toVelocity $ fromIntegral v)
            Node i [Number c, Number p, Number v] | name i == "Off" ->
                sendNote sq Event.NoteOff (ChannelMsg.toChannel $ fromIntegral c)
                                       (ChannelMsg.toPitch $ fromIntegral p)
                                          (ChannelMsg.toVelocity $ fromIntegral v)
            Node i [Number c, Number p, Number v] | name i == "PgmChange" ->
                sendEvent sq $ Event.CtrlEv Event.PgmChange $ Event.Ctrl
                               (fromIntegral c) (fromIntegral p) (fromIntegral v)
            _ -> error $ "Event.play_event: missing case for: " ++ show x

sendNote :: Sequencer SndSeq.OutputMode
         -> Event.NoteEv
            -> ChannelMsg.Channel -> ChannelMsg.Pitch -> ChannelMsg.Velocity
            -> IO ()
sendNote h onoff chan pitch velocity = do
  sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
