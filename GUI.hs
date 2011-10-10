-- module GUI where

{-# LANGUAGE PatternSignatures #-}

import qualified IO
import Term
import Program
import Rewrite

import Graphics.UI.WX as WX
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Event
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad ( forever, void )
import Text.Parsec ( parse )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, hSetBuffering, BufferMode(..), stderr )

-- | read rules file, should contain definition for "main"
main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    [ f ] <- getArgs
    s <- readFile f
    input <- newChan
    writeChan input s
    output <- newChan
    void $ forkIO $
        withSequencer "Rewrite-Sequencer" $ machine input output s
    gui input output s

machine ::
    Chan String ->
    Chan String ->
    String ->
    Sequencer SndSeq.OutputMode ->
    IO ()
machine input output sinit sq = do
    program :: MVar Program <- newMVar $ read sinit
    void $ forkIO $ forever $ do
        s <- readChan input
        hPutStrLn stderr $ "new input\n" ++ s
        case parse IO.input "editor" s of
            Left err -> print err
            Right ( p :: Program ) -> do
                hPutStrLn stderr "parser OK"
                void $ swapMVar program p
                hPutStrLn stderr "swapped OK"
    execute program ( read "main" ) output sq

execute ::
   MVar Program ->
   Term ->
   Chan String ->
   Sequencer SndSeq.OutputMode ->
   IO ()
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
        _ -> error $ "GUI.execute: invalid stream\n" ++ show s

gui :: Chan String -> Chan String -> String -> IO ()
gui input output sinit = WX.start $ do
    putStrLn "frame"
    f <- WX.frame [ text := "f" ]
    putStrLn "panel"
    p <- WX.panel f [ ]

    -- continue <- WX.button p [ text := "continue" ]
    -- pause <- WX.button p [ text := "pause" ]
    -- reset <- WX.button p [ text := "reset" ]
    editor <- textCtrl p [ text := sinit ]
    set editor [ on enterKey :=
                      writeChan input =<< get editor text ]
    tracer <- textCtrl p []
    void $ forkIO $ forever $ do
        s <- readChan output
        hPutStrLn stderr s
        -- following line is what is intended
        -- (show string in text widget)
        -- but it will crash wx
        --    set tracer [ text := s ]
    set f [ layout := container p $ margin 10
            $ column 5 $ map WX.fill
            [ widget editor
            -- , widget continue, widget pause, widget reset
            , widget tracer
            ]
          ]
    putStrLn "return"
