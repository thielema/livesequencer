-- module GUI where

{-# language PatternSignatures #-}

import IO
import Term
import Program
import Rewrite

import Graphics.UI.WX as WX
import Control.Concurrent
import Control.Concurrent.MVar

import Event
import Common

import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.ALSA as MidiAlsa

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad ( forever, void )
import Text.Parsec ( parse )
import System.Environment
import System.IO 

-- | read rules file, should contain definition for "main"
main = do
    hSetBuffering stderr LineBuffering
    [ f ] <- getArgs
    s <- readFile f
    input <- newChan
    writeChan input s
    output <- newChan
    forkIO $ withSequencer "Mode" $ machine input output s
    WX.start $ gui input output s
    
machine input output s sq = do
    program :: MVar Program <- newMVar $ read s
    forkIO $ forever $ do
        s <- readChan input
        hPutStrLn stderr $ "new input\n" ++ s
        case parse IO.input "editor" s of
            Left err -> print err
            Right ( p :: Program ) -> do
                hPutStrLn stderr "parser OK"
                void $ swapMVar program p
                hPutStrLn stderr "swapped OK"
    execute program ( read "main" ) output sq

execute ( program :: MVar Program ) t output sq = do 
    hPutStrLn stderr "execute"                
    p :: Program <- readMVar program -- this happens anew at each click 
                          -- since the program text might have changed in the editor
    hPutStrLn stderr "got program from MVar"
    let s = force_head p t
    writeChan output $ show s
    case s of
        Node (Identifier "Nil") [] -> do
            hPutStrLn stderr "finished."
        Node (Identifier "Cons") [x, xs] -> do
            play_event x sq
            execute program xs output sq
        
gui input output s = WX.start $ do
    f <- WX.frame [ text := "f" ]
    p <- WX.panel f [ ]
    
    -- continue <- WX.button p [ text := "continue" ]
    -- pause <- WX.button p [ text := "pause" ] 
    -- reset <- WX.button p [ text := "reset" ]
    editor <- textCtrl p [ text := s ] 
    set editor [ on enterKey := do
                      s <- get editor text
                      writeChan input s
                      ]
    tracer <- textCtrl p []
    forkIO $ forever $ do
        s <- readChan output
        hPutStrLn stderr s
        -- following line is what is intended
        -- (show string in text widget)
        -- but it will crash wx
        --    set tracer [ text := s ]
    set f [ layout := container p $ margin 10 
            $ column 5 $ map WX.hfill 
            [ widget editor
            -- , widget continue, widget pause, widget reset            
            , widget tracer 
            ] 
          ]
    return ()
    
    
