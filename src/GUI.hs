-- module GUI where

{-# LANGUAGE ScopedTypeVariables #-}

import qualified IO
import Term
import Program
import Rewrite

import Graphics.UI.WX as WX
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import qualified Graphics.UI.WXCore as WXCore
import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )
import Graphics.UI.WXCore.WxcClassesMZ ( wxEVT_COMMAND_MENU_SELECTED )
import Graphics.UI.WXCore.WxcClassesAL ( commandEventCreate, evtHandlerAddPendingEvent )

import Graphics.UI.WXCore.Events
import Event
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.Trans.Writer ( runWriter )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forever, forM, forM_ )
import Text.Parsec ( parse )
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, hSetBuffering, BufferMode(..), stderr )

import qualified Data.Map as M

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    fs <- getArgs
    ss <- forM fs readFile
    let pack = M.fromList $ zip fs ss
    let parsed_pack = flip M.mapWithKey pack $ \ f s -> case parse IO.input f s of
            Left err -> error $ show err
            Right p  -> (s,p)
    input <- newChan
    output <- newChan
    void $ forkIO $
        withSequencer "Rewrite-Sequencer" $ machine input output $ parsed_pack
    gui input output pack


machine :: Chan (FilePath, String) -- ^ machine reads program text from here
        -> Chan (M.Map FilePath String, [Message], String) -- ^ and writes output to here
        -> ( M.Map FilePath (String, Program) ) -- ^ initial program
        -> Sequencer SndSeq.DuplexMode
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
                modifyMVar_ package $ return . M.insert f (s,p)
                hPutStrLn stderr "modified OK"

    startQueue sq
    MS.evalStateT
       ( execute package ( read "main" ) ( writeChan output ) sq ) 0


-- | following code taken from http://snipplr.com/view/17538/
myEventId :: Int
myEventId = wxID_HIGHEST+1 -- the custom event ID

-- | the custom event is registered as a menu event
createMyEvent :: IO (WXCore.CommandEvent ())
createMyEvent = commandEventCreate wxEVT_COMMAND_MENU_SELECTED myEventId

registerMyEvent :: WXCore.EvtHandler a -> IO () -> IO ()
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io


execute :: MVar ( M.Map FilePath (String,Program) )
                  -- ^ current program (GUI might change the contents)
        -> Term -- ^ current term
        -> ( (M.Map FilePath String, [Message], String) -> IO () ) -- ^ sink for messages (show current term)
        -> Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> MS.StateT Time IO ()
execute program t output sq = do
    -- hPutStrLn stderr "execute"
    pa <- liftIO $ readMVar program
                          -- this happens anew at each click
                          -- since the program text might have changed in the editor
    -- hPutStrLn stderr "got program from MVar"
    let p = Program { rules = concat $ map (rules.snd) $ M.elems pa }
    let ( s, log ) = runWriter $ force_head p t
    liftIO $ output $ ( fmap fst pa, log, show s )
    case s of
        Node i [] | name i == "Nil" -> do
            liftIO $ hPutStrLn stderr "finished."
        Node i [x, xs] | name i == "Cons" -> do
            play_event x sq
            execute program xs output sq
        _ -> error $ "GUI.execute: invalid stream\n" ++ show s

gui :: Chan (FilePath, String) -- ^  the gui writes here
      -- (if the program text changes due to an edit action)
    -> Chan (M.Map FilePath String, [Message], String) -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> M.Map FilePath String -- ^ initial texts for modules
    -> IO ()
gui input output pack = WX.start $ do
    f <- WX.frame
        [ text := "live-sequencer", visible := False
        ]

    out <- varCreate (M.empty, [], "reduction")

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

    panelsHls <- forM (M.toList pack) $ \ (path,content) -> do
        psub <- panel nb []
        editor <- textCtrl psub [ font := fontFixed ]
        highlighter <- textCtrl psub [ ]
        -- TODO: show status (modified in editor, sent to machine, saved to file)
        -- TODO: load/save actions
        set editor [ text := content
                   , on enterKey := do
                       s <- get editor text
                       writeChan input (path,s)
                   ]
        return
           (tab path $ container psub $ column 5 $
               map WX.fill $ [widget editor, widget highlighter],
            (path,highlighter))
    let panels = map fst panelsHls
        highlighters = M.fromList $ map snd panelsHls

    reducer <- textCtrl p [ font := fontFixed ]

    registerMyEvent f $ do
        -- putStrLn "The custom event is fired!!"
        (contents,sh,sr) <- varGet out
        forM_ (M.toList contents) $ \(path,content) ->
            case M.lookup path highlighters of
                Nothing -> return ()
                Just highlighter ->
                    set highlighter [ text := content ]
        mapM_ print sh
--        set highlighter [ text := unlines ( map show sh ) ]
        set reducer [ text := sr ]

    set f [ layout := container p $ margin 5
            $ column 5 $ map WX.fill
            [ tabs nb panels
            , widget reducer
            ]
            , visible := True
            , clientSize := sz 500 300
          ]
