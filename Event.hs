module Event where

import Term
import Common (sendEvent, Sequencer )

import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Concurrent ( threadDelay )

play_event x sq = case x of    
            Node (Identifier "Wait") [Number n] ->
                threadDelay (fromIntegral n * 10^3)
            Node (Identifier "On") [Number c, Number p, Number v] ->
                sendNote sq Event.NoteOn (ChannelMsg.toChannel $ fromIntegral c) 
                          (ChannelMsg.toPitch $ fromIntegral p)
                                       (ChannelMsg.toVelocity $ fromIntegral v)
            Node (Identifier "Off") [Number c, Number p, Number v] ->
                sendNote sq Event.NoteOff (ChannelMsg.toChannel $ fromIntegral c) 
                                       (ChannelMsg.toPitch $ fromIntegral p)
                                          (ChannelMsg.toVelocity $ fromIntegral v)
            Node (Identifier "PgmChange") [Number c, Number p, Number v] ->
                sendEvent sq $ Event.CtrlEv Event.PgmChange $ Event.Ctrl
                               (fromIntegral c) (fromIntegral p) (fromIntegral v)

        
sendNote :: Sequencer SndSeq.OutputMode 
         -> Event.NoteEv
            -> ChannelMsg.Channel -> ChannelMsg.Pitch -> ChannelMsg.Velocity
            -> IO ()
sendNote h onoff chan pitch velocity = do
  sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch velocity velocity 0
