import Graphics.UI.WX as WX
import Control.Concurrent
import Control.Concurrent.MVar

import Common
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

main = do
    withSequencer "Mode" $ WX.start . gui



gui sq = WX.start $ do
    f <- WX.frame [ text := "f" ]
    WX.timer f [ WX.interval := 1000
               , on command := do 
                             sendNote sq Event.NoteOff (ChannelMsg.toChannel 0) 
                                         (ChannelMsg.toPitch 60)
                             sendNote sq Event.NoteOn (ChannelMsg.toChannel 0) 
                                         (ChannelMsg.toPitch 60)
               ]
    p <- WX.panel f [ ]
    b1 <- WX.button p [ text := "b1" ]
    b2 <- WX.button p 
          [ text := "b2" 
          , on command := 
            set b1 [ text := "set b1" ]  
          ]
    bm <- WX.button p [ text := "midi" 
          , on command := do
            sendMode sq (ChannelMsg.toChannel 0) Mode.AllNotesOff
          ]  
    txt <- textCtrl p [] 
    set txt [ on enterKey := do
                      s <- get txt text
                      print s
                      ]
    set f [ layout := container p $ margin 10 
            $ column 5 $ map WX.hfill 
            [ widget b1, widget b2, widget bm, widget txt ] 
          ]
    return ()
    
    
sendMode :: Sequencer SndSeq.OutputMode -> ChannelMsg.Channel -> Mode.T -> IO ()
sendMode h chan mode = do
  sendEvent h $
    Event.CtrlEv Event.Controller $ MidiAlsa.modeEvent chan mode
               
               
sendNote :: Sequencer SndSeq.OutputMode 
         -> Event.NoteEv
            -> ChannelMsg.Channel -> ChannelMsg.Pitch -> IO ()
sendNote h onoff chan pitch = do
  sendEvent h $
    Event.NoteEv onoff $ MidiAlsa.noteEvent chan pitch 
        ( ChannelMsg.toVelocity 64 ) ( ChannelMsg.toVelocity 64 ) 0
        
               
