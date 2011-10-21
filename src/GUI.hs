-- module GUI where

{-# LANGUAGE ScopedTypeVariables #-}

import qualified IO
import Term
import Module ( Module, source_location, source_text )
import Program
import Rewrite
import qualified Option

import Graphics.UI.WX as WX
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import qualified Graphics.UI.WXCore as WXCore
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )
import Graphics.UI.WXCore.WxcClassesAL ( commandEventCreate, evtHandlerAddPendingEvent )

import Graphics.UI.WXCore.Events
import Event
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import Control.Monad.Trans.Writer ( runWriter )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forever, forM, forM_ )
import Text.ParserCombinators.Parsec ( parse )
import qualified Text.ParserCombinators.Parsec as Pos

import System.IO ( hPutStrLn, hSetBuffering, BufferMode(..), stderr )

import qualified Data.Map as M
import Data.Maybe ( maybeToList, fromMaybe )

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    opt <- Option.get
    p <- Program.chase (Option.importPaths opt) $ Option.moduleName opt

    input <- newChan
    output <- newChan
    void $ forkIO $
        withSequencer "Rewrite-Sequencer" $ machine input output $ p
    gui input output p


machine :: Chan (Identifier, String) -- ^ machine reads program text from here
                   -- (module name, module contents)
        -> Chan ( [Message], String) -- ^ and writes output to here
                   -- (log message (for highlighting), current term)
        -> Program -- ^ initial program
        -> Sequencer SndSeq.DuplexMode
        -> IO ()
machine input output pack sq = do
    package <- newMVar pack
    void $ forkIO $ forever $ do
        (f, s) <- readChan input
        hPutStrLn stderr $ "module " ++ show f ++ " has new input\n" ++ s
        case parse IO.input ( show f ) s of
            Left err -> print err
            Right ( m0 :: Module ) -> do
                -- TODO: handle the case that the module changed its name
                -- (might happen if user changes the text in the editor)
                p <- readMVar package
                let Just previous = M.lookup f $ modules p
                let m = m0 { source_location = source_location previous
                           , source_text = s 
                           }
                hPutStrLn stderr "parser OK"
                modifyMVar_ package $ \ p -> 
                    return $ p { modules =  M.insert f m $ modules p }
                hPutStrLn stderr "modified OK"

    startQueue sq
    MS.evalStateT
       ( execute package ( read "main" ) ( writeChan output ) sq ) 0


-- | following code taken from http://snipplr.com/view/17538/
myEventId :: Int
myEventId = wxID_HIGHEST+1 -- the custom event ID

-- | the custom event is registered as a menu event
createMyEvent :: IO (WXCore.CommandEvent ())
createMyEvent = commandEventCreate WXCMZ.wxEVT_COMMAND_MENU_SELECTED myEventId

registerMyEvent :: WXCore.EvtHandler a -> IO () -> IO ()
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io


execute :: MVar Program
                  -- ^ current program (GUI might change the contents)
        -> Term -- ^ current term
        -> ( ( [Message], String) -> IO () ) -- ^ sink for messages (show current term)
        -> Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> MS.StateT Time IO ()
execute program t output sq = do
    -- hPutStrLn stderr "execute"
    p <- liftIO $ readMVar program
                          -- this happens anew at each click
                          -- since the program text might have changed in the editor
    -- hPutStrLn stderr "got program from MVar"
    let ( s, log ) = runWriter $ force_head p t
    liftIO $ output $ ( log, show s )
    case s of
        Node i [] | name i == "[]" -> do
            liftIO $ hPutStrLn stderr "finished."
        Node i [x, xs] | name i == ":" -> do
            play_event x sq
            case x of 
                Node j _ | name j == "Wait" -> do
                    pa <- liftIO $ readMVar program
                    liftIO $ output ( [ Reset_Display ], "Reset_Display" )
                _ -> return ()
            execute program xs output sq
        _ -> error $ "GUI.execute: invalid stream\n" ++ show s


-- might be moved to wx package
cursor :: Attr (TextCtrl a) Int
cursor =
    newAttr "cursor"
        WXCMZ.textCtrlGetInsertionPoint
        WXCMZ.textCtrlSetInsertionPoint

editable :: WriteAttr (TextCtrl a) Bool
editable =
    writeAttr "editable"
        WXCMZ.textCtrlSetEditable


gui :: Chan (Identifier, String) -- ^  the gui writes here
      -- (if the program text changes due to an edit action)
    -> Chan ([Message], String) -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> Program -- ^ initial texts for modules
    -> IO ()
gui input output pack = WX.start $ do
    f <- WX.frame
        [ text := "live-sequencer", visible := False
        ]

    out <- newChan

    void $ forkIO $ forever $ do
        s <- readChan output
        writeChan out s
        ev <- createMyEvent
        evtHandlerAddPendingEvent f ev

    p <- WX.panel f [ ]
    nb <- WX.notebook p [ ]

    let refreshProgram (path, editor, highlighter) = do
            s <- get editor text
            pos <- get editor cursor
            set highlighter [ text := s, cursor := pos ]
            writeChan input (path,s)
    -- TODO: control the sequencer:
    -- continue <- WX.button p [ text := "continue" ]
    -- pause <- WX.button p [ text := "pause" ]
    -- reset <- WX.button p [ text := "reset" ]

    panelsHls <- forM (M.toList $ modules pack) $ \ (path,content) -> do
        psub <- panel nb []
        editor <- textCtrl psub [ font := fontFixed ]
        highlighter <- textCtrlRich psub [ font := fontFixed, editable := False ]
        -- TODO: show status (modified in editor, sent to machine, saved to file)
        -- TODO: load/save actions
        set editor [ text := source_text content
                   , on enterKey := refreshProgram (path, editor, highlighter)
                   ]
        set highlighter [ text := source_text content ]
        return
           (tab ( show path ) $ container psub $ column 5 $
               map WX.fill $ [widget editor, widget highlighter],
            (path, editor, highlighter))

    let panels = map fst panelsHls
        highlighters = M.fromList $ map ( \ (_,(pnl,_,h)) -> (pnl, h) ) panelsHls

    refreshButton <- WX.button p
        [ text := "refresh",
          on command := mapM_ (refreshProgram . snd) panelsHls ]
    reducer <- textCtrl p [ font := fontFixed, editable := False ]


    highlights <- varCreate M.empty

    registerMyEvent f $ do
        (log,sr) <- readChan out
        let setColorHighlighters ::
                M.Map Identifier [Identifier] -> Int -> Int -> Int -> IO ()
            setColorHighlighters m r g b =
                set_color nb highlighters m ( rgb r g b )
        void $ forM log $ \ msg -> do
          case msg of
            Step { } -> do
                set reducer [ text := sr ]

                let m = M.fromList $ do
                      t <- target msg : maybeToList ( Rewrite.rule msg )
                      (ident,_) <-
                          reads $ Pos.sourceName $ Term.start t
                      return (ident, [t])
                void $ varUpdate highlights $ M.unionWith (++) m
                setColorHighlighters m 0 200 200

            Data { } -> do
                set reducer [ text := sr ]

                let t = origin msg
                    m = M.fromList $ do
                      (ident,_) <-
                          reads $ Pos.sourceName $ Term.start t
                      return (ident, [t])
                void $ varUpdate highlights $ M.unionWith (++) m
                setColorHighlighters m 200 200 0

            Reset_Display -> do
                previous <- varSwap highlights M.empty
                setColorHighlighters previous 255 255 255

    set f [ layout := container p $ margin 5
            $ column 5
            [ WX.hfill $ row 5 $ map widget [refreshButton]
            , WX.fill $ tabs nb panels
            , WX.fill $ widget reducer
            ]
            , visible := True
            , clientSize := sz 500 300
          ]

set_color ::
    (Ord k) =>
    Notebook a ->
    M.Map k (TextCtrl a) ->
    M.Map k [Identifier] ->
    Color ->
    IO ()
set_color nb highlighters positions hicolor = void $ do
    index <- WXCMZ.notebookGetSelection nb
    let (p, highlighter) = M.toList highlighters !! index
    attr <- WXCMZ.textCtrlGetDefaultStyle highlighter
    bracket
        (WXCMZ.textAttrGetBackgroundColour attr)
        (WXCMZ.textAttrSetBackgroundColour attr) $ const $ do
            WXCMZ.textAttrSetBackgroundColour attr hicolor
            forM_ (fromMaybe [] $ M.lookup p positions) $ \ ident -> do
                let mkpos pos =
                       WXCMZ.textCtrlXYToPosition highlighter
                          $ Point (Pos.sourceColumn pos - 1)
                                  (Pos.sourceLine   pos - 1)
                from <- mkpos $ Term.start ident
                to   <- mkpos $ Term.end ident
                WXCMZ.textCtrlSetStyle highlighter from to attr
