-- module Console where

import IO
import Term
import Program
import Rewrite

import Common
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Text.Parsec
import System.Environment
import Control.Monad ( forM )

import Control.Concurrent

-- | read rules file, start expansion of "main"
main = do
    [ f ] <- getArgs
    s <- readFile f
    case parse input f s of
        Left err -> print err
        Right p -> withSequencer "Mode" $ execute ( rules p ) ( read "main" )
      
execute rs t sq = do      
    let s = normal rs t
    print s
    case s of
        Node (Identifier "Nil") [] -> return ()
        Node (Identifier "Cons") [x, xs] -> do
          case x of    
            Node (Identifier "Wait") [Number n] ->
                threadDelay (fromIntegral n * 10^3)
            Node (Identifier "On") [Number n] ->
                sendNote sq Event.NoteOn (ChannelMsg.toChannel 0) 
                                       (ChannelMsg.toPitch $ fromIntegral n)
            Node (Identifier "Off") [Number n] ->
                sendNote sq Event.NoteOn (ChannelMsg.toChannel 0) 
                                       (ChannelMsg.toPitch $ fromIntegral n)
          execute rs xs sq
        
sendNote :: Sequencer SndSeq.OutputMode 
         -> Event.NoteEv
            -> ChannelMsg.Channel -> ChannelMsg.Pitch -> IO ()
sendNote h onoff chan pitch = do
  sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch 
        ( ChannelMsg.toVelocity 64 ) ( ChannelMsg.toVelocity 64 ) 0
      