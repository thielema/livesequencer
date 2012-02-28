module Render where

import qualified Midi

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import Data.Monoid (mempty, mappend, )


class Message msg where
   makeMessage :: msg -> ChannelMsg.Body

class ChannelMessage msg where
   makeChannelMessage :: msg -> ChannelMsg.T

instance ChannelMessage Midi.Message where
   makeChannelMessage =
      ChannelMsg.Cons (ChannelMsg.toChannel 0) . makeMessage

instance Message Midi.Message where
   makeMessage msg =
      ChannelMsg.Voice $
      case msg of
         Midi.On  pitch velocity ->
            VoiceMsg.NoteOn
               (VoiceMsg.toPitch $ fromInteger pitch)
               (VoiceMsg.toVelocity $ fromInteger velocity)
         Midi.Off pitch velocity ->
            VoiceMsg.NoteOff
               (VoiceMsg.toPitch $ fromInteger pitch)
               (VoiceMsg.toVelocity $ fromInteger velocity)
         Midi.PgmChange pgm ->
            VoiceMsg.ProgramChange
               (VoiceMsg.toProgram $ fromInteger pgm)
         Midi.Controller ctrl value ->
            VoiceMsg.Control
               (VoiceMsg.toController $ fromInteger ctrl)
               (fromInteger value)

instance Message msg => ChannelMessage (Midi.Channel msg) where
   makeChannelMessage (Midi.Channel chan msg) =
      ChannelMsg.Cons (ChannelMsg.toChannel $ fromInteger chan) $
      makeMessage msg


trackFromStream ::
   (ChannelMessage msg) => [Midi.Event msg] -> MidiFile.Track
trackFromStream evs =
   foldr
      (\ev go time ->
         case ev of
            Midi.Wait pause ->
               go (mappend time $
                   NonNeg.fromNumberMsg "Render.trackFromStream: Wait" pause)
            Midi.Say str ->
               EventList.cons time (FileEvent.MetaEvent $ MetaEvent.Lyric str) $
               go 0
            Midi.Event msg ->
               EventList.cons time (FileEvent.MIDIEvent $ makeChannelMessage msg) $
               go 0)
      (\ _time -> EventList.empty) evs mempty

fileFromStream ::
   (ChannelMessage msg) => [Midi.Event msg] -> MidiFile.T
fileFromStream =
   MidiFile.Cons MidiFile.Mixed (MidiFile.Ticks 500) .
   (:[]) .
   -- EventList.cons 0 (MetaEvent.SetTempo 500000) .
   trackFromStream

writeStream ::
   (ChannelMessage msg) => FilePath -> [Midi.Event msg] -> IO ()
writeStream path =
   Save.toFile path . fileFromStream
