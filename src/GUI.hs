{-# LANGUAGE ScopedTypeVariables #-}

import qualified IO
import Term
import Program
import Rewrite

import Graphics.UI.WX as WX
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )
import Graphics.UI.WXCore.WxcClassesMZ ( wxEVT_COMMAND_MENU_SELECTED )
import Graphics.UI.WXCore.WxcClassesAL ( commandEventCreate, evtHandlerAddPendingEvent )

import Graphics.UI.WXCore.Events 
import Event
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad ( forever, forM )
import Control.Monad.Writer
import Text.Parsec ( parse )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, hSetBuffering, BufferMode(..), stderr )

import qualified Data.Map as M

-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    fs <- getArgs
    ss <- forM fs readFile
    let pack = M.fromList $ zip fs ss
    let parsed_pack = flip M.mapWithKey pack $ \ f s -> case parse IO.input f s of
            Left err -> error $ show err
            Right p  -> p
    input <- newChan
    output <- newChan
    void $ forkIO $
        withSequencer "Rewrite-Sequencer" $ machine input output $ parsed_pack
    gui input output pack


machine :: Chan (FilePath, String) -- ^ machine reads program text from here
        -> Chan String -- ^ and writes output to here
        -> ( M.Map FilePath Program ) -- ^ initial program 
        -> Sequencer SndSeq.OutputMode 
        -> IO ()
machine input output pack sq = do
    package <- newMVar pack
    void $ forkIO $ forever $ do
        (f, s) <- readChan input
        hPutStrLn stderr $ "module " ++ f ++ " has new input\n" ++ s
        case parse IO.input f s of
            Left err -> print err
            Right ( p :: Program ) -> do
                hPutStrLn stderr "parser OK"
                modifyMVar_ package $ \ pack -> return $ M.insert f p pack
                hPutStrLn stderr "modified OK"
    execute package ( read "main" ) ( writeChan output ) sq

-- | following code taken from http://snipplr.com/view/17538/
myEventId = wxID_HIGHEST+1 -- the custom event ID
-- | the custom event is registered as a menu event
createMyEvent = commandEventCreate wxEVT_COMMAND_MENU_SELECTED myEventId
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io
 

execute :: MVar ( M.Map FilePath Program ) 
                  -- ^ current program (GUI might change the contents)
        -> Term -- ^ current term
        -> ( String -> IO () ) -- ^ sink for messages (show current term)
        -> Sequencer SndSeq.OutputMode -- ^ for playing MIDI events
        -> IO ()
execute program t output sq = do
    -- hPutStrLn stderr "execute"
    pa <- readMVar program -- this happens anew at each click
                          -- since the program text might have changed in the editor
    -- hPutStrLn stderr "got program from MVar"
    let p = Program { rules = concat $ map rules $ M.elems pa }
    let ( s, log ) = runWriter $ force_head p t
    output $ unlines $ map show log ++ [ "--", show s  ]
    case s of
        Node i [] | name i == "Nil" -> do
            hPutStrLn stderr "finished."
        Node i [x, xs] | name i == "Cons" -> do
            play_event x sq
            execute program xs output sq
        _ -> error $ "GUI.execute: invalid stream\n" ++ show s

gui :: Chan (FilePath, String) -- ^  the gui writes here 
      -- (if the program text changes due to an edit action)
    -> Chan String -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> M.Map FilePath String -- ^ initial texts for modules
    -> IO ()
gui input output pack = WX.start $ do
    putStrLn "frame"
    f <- WX.frame 
        [ text := "live-sequencer", visible := False 
        ]

    out <- varCreate "output"

    void $ forkIO $ forever $ do
        s <- readChan output
        varSet out s
        ev <- createMyEvent
        evtHandlerAddPendingEvent f ev

    p <- WX.panel f [ ]
    nb <- WX.notebook p [ ]

    -- TODO: control the sequencer:
    -- continue <- WX.button p [ text := "continue" ]
    -- pause <- WX.button p [ text := "pause" ]
    -- reset <- WX.button p [ text := "reset" ]

    panels <- forM (M.toList pack) $ \ (f, s) -> do
        psub <- panel nb []
        editor <- textCtrl psub [ font := fontFixed ]
        -- TODO: show status (modified in editor, sent to machine, saved to file)
        -- TODO: load/save actions
        set editor [ text := s
                   , on enterKey := do
                       s <- get editor text 
                       writeChan input (f, s)
                   ]
        return $ tab f  $ container psub $ WX.fill $ widget editor

    tracer <- staticText p [ font := fontFixed ]

    registerMyEvent f $ do
        -- putStrLn "The custom event is fired!!"
        s <- varGet out
        set tracer [ text := s ]

    set f [ layout := container p $ margin 5
            $ column 5 $ map WX.fill
            [ tabs nb panels
            , widget tracer
            ]
            , visible := True
            , clientSize := sz 500 300
          ]

