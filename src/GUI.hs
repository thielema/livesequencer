-- module GUI where

import qualified IO
import Term
import Program
import qualified Module
import qualified Rewrite
import qualified Option

import qualified Controls

import Graphics.UI.WX as WX
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Control.Monad.STM as STM

import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef )

import qualified Graphics.UI.WXCore as WXCore
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )
import Graphics.UI.WXCore.WxcClassesAL ( commandEventCreate, evtHandlerAddPendingEvent )

import Graphics.UI.WXCore.Events
import Event
import Common

import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( forever, forM, forM_ )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Pos as Pos

import System.IO ( hPutStrLn, hSetBuffering, BufferMode(..), stderr )
import qualified System.Exit as Exit

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Maybe ( maybeToList, fromMaybe )

import qualified Data.List as List
import Data.Bool.HT ( if' )

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    hSetBuffering stderr LineBuffering
    opt <- Option.get

    (p,ctrls) <-
        Exc.resolveT (\e -> hPutStrLn stderr (show e) >> Exit.exitFailure) $ do
            p0 <- Program.chase (Option.importPaths opt) $ Option.moduleName opt
            let ctrls = Controls.collect p0
            p1 <- Exc.ExceptionalT $ return $
                Program.add_module (Controls.controller_module ctrls) p0
            return (p1, ctrls)

    input <- newChan
    output <- newChan
    withSequencer "Rewrite-Sequencer" $ \sq ->
        flip finally (stopQueue sq) $ WX.start $ do
            gui ctrls input output p
            void $ forkIO $ machine input output p sq

-- | messages that are sent from GUI to machine
data Action =
     Modification Identifier String Int -- ^ modulename, sourcetext, position
   | Execution Execution
   | Control Controls.Event

data Execution = Restart | Stop | Pause | Continue


data ExceptionType = ParseException | TermException
    deriving (Show, Eq, Ord, Enum)

-- | messages that are sent from machine to GUI
data GuiUpdate =
     Term { _steps :: [ Rewrite.Message ], _currentTerm :: String }
   | Exception { _excType :: ExceptionType, _range :: Range, _message :: String }
   | Refresh { _moduleName :: Identifier, _content :: String, _position :: Int }
   | Running Bool
   | ResetDisplay


type ExceptionItem = (ExceptionType, Range, String)

lineFromExceptionItem :: ExceptionItem -> [String]
lineFromExceptionItem (typ, rng, descr) =
    case rng of
        Range pos _ ->
            Pos.sourceName pos :
            show (Pos.sourceLine pos) : show (Pos.sourceColumn pos) :
            (case typ of
                ParseException -> "parse error"
                TermException -> "term error") :
            descr :
            []


writeTMVar :: TMVar a -> a -> STM.STM ()
writeTMVar var a =
    tryTakeTMVar var >> putTMVar var a

{-
This runs concurrently
and is fed with changes to the modules by the GUI.
It parses them and provides the parsed modules to the execution engine.
Since parsing is a bit of work
we can keep the GUI and the execution of code going while parsing.
-}
machine :: Chan Action -- ^ machine reads program text from here
                   -- (module name, module contents)
        -> Chan GuiUpdate -- ^ and writes output to here
                   -- (log message (for highlighting), current term)
        -> Program -- ^ initial program
        -> Sequencer SndSeq.DuplexMode
        -> IO ()
machine input output prog sq = do
    program <- newTVarIO prog
    let mainName = read "main"
    term <- newTMVarIO mainName

    void $ forkIO $ flip MS.evalStateT mainName $ forever $ do
        action <- liftIO $ readChan input
        let running =
                liftIO . writeChan output . Running
        case action of
            Control event -> liftIO $ do
                hPutStrLn stderr $ show event
                Exc.resolveT
                    (writeChan output .
                     uncurry (Exception ParseException)) $
                    Exc.mapExceptionalT STM.atomically $ do
                        p <- lift $ readTVar program
                        p' <-
                            Exc.ExceptionalT $ return $
                            Controls.change_controller_module p event
                        lift $ writeTVar program p'
                        -- return $ Controls.get_controller_module p'
                -- hPutStrLn stderr $ show m
            Execution exec ->
                case exec of
                    Restart -> do
                        running True
                        MS.put mainName
                        liftIO $ quietContinueQueue sq
                        liftIO $ void $ STM.atomically $ writeTMVar term mainName
                    Stop -> do
                        running False
                        liftIO $ stopQueue sq
                        liftIO $ void $ STM.atomically $ tryTakeTMVar term
                        MS.put mainName
                    Pause -> do
                        running False
                        liftIO $ pauseQueue sq
                        MS.put .
                            maybe mainName id
                            =<< (liftIO $ STM.atomically $ tryTakeTMVar term)
                    Continue -> do
                        running True
                        liftIO $ continueQueue sq
                        liftIO . STM.atomically . writeTMVar term
                            =<< MS.get
                        MS.put mainName
            Modification moduleName sourceCode pos -> liftIO $ do
                hPutStrLn stderr $
                    "module " ++ show moduleName ++
                    " has new input\n" ++ sourceCode
                Exc.resolveT
                    (writeChan output .
                     uncurry (Exception ParseException)) $ do
                    m0 <-
                        Exc.mapExceptionT Program.messageFromParserError $
                        Exc.fromEitherT $ return $
                        Parsec.parse IO.input ( show moduleName ) sourceCode
                    Exc.mapExceptionalT STM.atomically $ do
                        -- TODO: handle the case that the module changed its name
                        -- (might happen if user changes the text in the editor)
                        p <- lift $ readTVar program
                        let Just previous = M.lookup moduleName $ modules p
                        let m = m0 { Module.source_location =
                                         Module.source_location previous
                                   , Module.source_text = sourceCode
                                   -- for now ignore renaming
                                   , Module.name = moduleName
                                   }
                        lift . writeTVar program =<<
                            (Exc.ExceptionalT $ return $
                             Program.add_module m p)
                    lift $ writeChan output
                        (Refresh moduleName sourceCode pos)
                    lift $ hPutStrLn stderr "parsed and modified OK"

    startQueue sq
    MS.evalStateT
        ( execute program term ( writeChan output ) sq ) 0


execute :: TVar Program
                  -- ^ current program (GUI might change the contents)
        -> TMVar Term -- ^ current term
        -> ( GuiUpdate -> IO () ) -- ^ sink for messages (show current term)
        -> Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> MS.StateT Time IO ()
execute program term output sq = forever $ do
    -- hPutStrLn stderr "execute"
    p <- liftIO $ readTVarIO program
        -- this happens anew at each click
        -- since the program text might have changed in the editor
    ( ( es, log ), result ) <- liftIO $ STM.atomically $ do
        eslog@( es, _log ) <-
            fmap (flip Rewrite.runEval p . Rewrite.force_head) $
            takeTMVar term
        let returnExc pos =
                return . Exc.Exception . (,) pos
        fmap ((,) eslog) $
            case es of
                Exc.Exception (pos,msg) -> returnExc pos msg
                Exc.Success s ->
                    case s of
                        Node i [x, xs] | name i == ":" -> do
                            putTMVar term xs
                            return $ Exc.Success x
                        Node i [] | name i == "[]" ->
                            returnExc (Term.range i) "finished."
                        _ ->
                            returnExc (Term.termRange s) $
                            "I do not know how to handle this term: " ++ show s

    liftIO $ Exc.switch (const $ return ()) (output . Term log . show) es

    case result of
        Exc.Exception (rng,msg) -> liftIO $ do
            stopQueue sq
            output $ Exception TermException rng msg
            output $ Running False
        Exc.Success x -> do
            mapM_ (liftIO . output . uncurry (Exception TermException))
                =<< play_event x sq
            case x of
                Node j _ | name j == "Wait" -> liftIO $
                    output ResetDisplay
                _ -> return ()


-- | following code taken from http://snipplr.com/view/17538/
myEventId :: Int
myEventId = wxID_HIGHEST+1 -- the custom event ID

-- | the custom event is registered as a menu event
createMyEvent :: IO (WXCore.CommandEvent ())
createMyEvent = commandEventCreate WXCMZ.wxEVT_COMMAND_MENU_SELECTED myEventId

registerMyEvent :: WXCore.EvtHandler a -> IO () -> IO ()
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io


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


notebookSelection :: Attr (Notebook a) Int
notebookSelection =
    newAttr "selection"
        WXCMZ.notebookGetSelection
        (\nb -> fmap (const ()) . WXCMZ.notebookSetSelection nb)


{-
The order of widget creation is important
for cycling through widgets using tabulator key.

gui :: Chan Action -- ^  the gui writes here
      -- (if the program text changes due to an edit action)
    -> Chan GuiUpdate -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> Program -- ^ initial texts for modules
    -> IO ()
-}

gui ctrls input output pack = do
    frameError <- WX.frame
        [ text := "errors", visible := False
        ]

    panelError <- WX.panel frameError [ ]

    errorLog <- WX.listCtrl panelError
        [ columns :=
              ("Module", AlignLeft, 120) :
              ("Row", AlignRight, -1) :
              ("Column", AlignRight, -1) :
              ("Type", AlignLeft, -1) :
              ("Description", AlignLeft, 500) :
              []
        ]
    errorList <- newIORef Seq.empty
    let refreshErrorLog = do
            errors <- readIORef errorList
            set errorLog [ items :=
                  map lineFromExceptionItem $ Fold.toList errors ]

    clearLog <- WX.button panelError
        [ text := "Clear",
          on command :=
              writeIORef errorList Seq.empty >> refreshErrorLog ]

    frameControls <- WX.frame [ text := "controls" ]
    panelControls <- WX.panel frameControls []

    Controls.create frameControls panelControls ctrls
        $ \ e -> writeChan input ( Control e )
    
    f <- WX.frame
        [ text := "live-sequencer", visible := False
        ]

    out <- newChan

    void $ forkIO $ forever $ do
        writeChan out =<< readChan output
        evtHandlerAddPendingEvent f =<< createMyEvent

    p <- WX.panel f [ ]

    let refreshProgram (path, editor, _highlighter) = do
            s <- get editor text
            pos <- get editor cursor
            writeChan input $ Modification path s pos

            modifyIORef errorList
                (Seq.filter
                    (\(_, errorRng, _) ->
                        name path /= Pos.sourceName (Term.start errorRng)))
            refreshErrorLog

    refreshButton <- WX.button p
        [ text := "Refresh",
          tooltip :=
              "parse the edited program and if successful\n" ++
              "replace the executed program\n" ++
              "shortcut: Ctrl-R" ]
    restartButton <- WX.button p
        [ text := "Restart",
          on command := writeChan input (Execution Restart),
          tooltip :=
              "restart program execution with 'main'\n" ++
              "shortcut: Ctrl-T" ]
    stopButton <- WX.button p
        [ text := "Stop",
          on command := writeChan input (Execution Stop),
          tooltip :=
              "stop program execution\n" ++
              "shortcut: Ctrl-Z" ]

    runningButton <- WX.checkBox p
        [ text := "running",
          checked := True,
          tooltip :=
              "pause or continue program execution\n" ++
              "shortcut: Ctrl-U" ]

    quitButton <- WX.button p
        [ text := "Quit" ]


    nb <- WX.notebook p [ ]

    panelsHls <- forM (M.toList $ modules pack) $ \ (path,content) -> do
        psub <- panel nb []
        editor <- textCtrl psub [ font := fontFixed, wrap := WrapNone ]
        highlighter <- textCtrlRich psub [ font := fontFixed, wrap := WrapNone, editable := False ]
        -- TODO: show status (modified in editor, sent to machine, saved to file)
        -- TODO: load/save actions
        let isRefreshKey k =
                -- Ctrl-R
                keyKey k == KeyChar '\018' && keyModifiers k == justControl
                -- Alt-R
                -- keyKey k == KeyChar 'r' && keyModifiers k == justAlt
            isRestartKey k =
                keyKey k == KeyChar '\020' && keyModifiers k == justControl
            isStopKey k =
                keyKey k == KeyChar '\026' && keyModifiers k == justControl
            isRunningKey k =
                keyKey k == KeyChar '\021' && keyModifiers k == justControl
        set editor
            [ text := Module.source_text content
            , on keyboard :~ \defaultHandler k ->
                 -- print (case keyKey k of KeyChar c -> fromEnum c) >>
                 if' (isRefreshKey k)
                     (refreshProgram (path, editor, highlighter)) $
                 if' (isRestartKey k)
                     (writeChan input (Execution Restart)) $
                 if' (isStopKey k)
                     (writeChan input (Execution Stop)) $
                 if' (isRunningKey k)
                     (do running <- get runningButton checked
                         writeChan input . Execution $
                             if' running Pause Continue) $
                 defaultHandler k
            ]
        set highlighter [ text := Module.source_text content ]
        return
           (tab ( show path ) $ container psub $ row 5 $
               map WX.fill $ [widget editor, widget highlighter],
            (path, editor, highlighter))

    let panels = map fst panelsHls
        highlighters = M.fromList $ map ( \ (_,(pnl,_,h)) -> (pnl, h) ) panelsHls
        editors = M.fromList $ map ( \ (_,(pnl,e,_)) -> (pnl, e) ) panelsHls


    set refreshButton
        [ on command := mapM_ (refreshProgram . snd) panelsHls ]

    set runningButton
        [ on command := do
              running <- get runningButton checked
              writeChan input . Execution $
                  if running then Continue else Pause ]


    reducer <-
        textCtrl p
            [ font := fontFixed, editable := False, wrap := WrapNone ]


    set f [ layout := container p $ margin 5
            $ column 5
            [ row 5 $
                  [widget refreshButton,
                   widget restartButton, widget stopButton, widget runningButton,
                   WX.hfill empty,
                   widget quitButton]
            , WX.fill $ tabs nb panels
            , WX.fill $ widget reducer
            ]
            , visible := True
            , clientSize := sz 500 300
          ]


    set errorLog
        [ on listEvent := \ev ->
              case ev of
                  ListItemSelected n -> do
                      errors <- readIORef errorList
                      let (typ, errorRng, _descr) = Seq.index errors n
                          moduleIdent = read $ Pos.sourceName $ Term.start errorRng
                      case List.elemIndex moduleIdent $ M.keys highlighters of
                          Nothing -> return ()
                          Just i -> set nb [ notebookSelection := i ]
                      let textField =
                              case typ of
                                  ParseException -> editors
                                  TermException -> highlighters
                      case M.lookup moduleIdent textField of
                          Nothing -> return ()
                          Just h -> do
                              i <- textPosFromSourcePos h $ Term.start errorRng
                              j <- textPosFromSourcePos h $ Term.end errorRng
                              set h [ cursor := i ]
                              WXCMZ.textCtrlSetSelection h i j
                  _ -> return ()
        ]

    set frameError
        [ layout := container panelError $ margin 5
              $ column 5 $
                 [ WX.fill $ widget errorLog,
                   WX.hfloatLeft $ widget clearLog ]
        , visible := True
        , clientSize := sz 500 300
        ]

    set quitButton
        [ on command := close f >> close frameError >> close frameControls ]


    highlights <- varCreate M.empty

    registerMyEvent f $ do
        msg <- readChan out
        let setColorHighlighters ::
                M.Map Identifier [Identifier] -> Int -> Int -> Int -> IO ()
            setColorHighlighters m r g b =
                set_color nb highlighters m ( rgb r g b )
        case msg of
            Term steps sr -> do
                set reducer [ text := sr, cursor := 0 ]
                forM_ steps $ \step ->
                  case step of
                    Rewrite.Step target mrule -> do

                        let m = M.fromList $ do
                              t <- target : maybeToList mrule
                              (ident,_) <-
                                  reads $ Pos.sourceName $ Term.start $ Term.range t
                              return (ident, [t])
                        void $ varUpdate highlights $ M.unionWith (++) m
                        setColorHighlighters m 0 200 200

                    Rewrite.Data origin -> do
                        set reducer [ text := sr, cursor := 0 ]

                        let m = M.fromList $ do
                              (ident,_) <-
                                  reads $ Pos.sourceName $ Term.start $ Term.range origin
                              return (ident, [origin])
                        void $ varUpdate highlights $ M.unionWith (++) m
                        setColorHighlighters m 200 200 0

            Exception typ pos descr -> do
                let exc = (typ, pos, descr)
                itemAppend errorLog $ lineFromExceptionItem exc
                modifyIORef errorList (Seq.|> exc)

            -- update highlighter text field only if parsing was successful
            Refresh path s pos -> do
                maybe (return ())
                    (\h -> set h [ text := s, cursor := pos ])
                    (M.lookup path highlighters)

            ResetDisplay -> do
                previous <- varSwap highlights M.empty
                setColorHighlighters previous 255 255 255

            Running running -> do
                set runningButton [ checked := running ]

textPosFromSourcePos ::
    TextCtrl a -> Pos.SourcePos -> IO Int
textPosFromSourcePos highlighter pos =
    WXCMZ.textCtrlXYToPosition highlighter
       $ Point (Pos.sourceColumn pos - 1)
               (Pos.sourceLine   pos - 1)

set_color ::
    (Ord k) =>
    Notebook a ->
    M.Map k (TextCtrl a) ->
    M.Map k [Identifier] ->
    Color ->
    IO ()
set_color nb highlighters positions hicolor = void $ do
    index <- get nb notebookSelection
    let (p, highlighter) = M.toList highlighters !! index
    attr <- WXCMZ.textCtrlGetDefaultStyle highlighter
    bracket
        (WXCMZ.textAttrGetBackgroundColour attr)
        (WXCMZ.textAttrSetBackgroundColour attr) $ const $ do
            WXCMZ.textAttrSetBackgroundColour attr hicolor
            forM_ (fromMaybe [] $ M.lookup p positions) $ \ ident -> do
                let rng = Term.range ident
                from <- textPosFromSourcePos highlighter $ Term.start rng
                to   <- textPosFromSourcePos highlighter $ Term.end rng
                WXCMZ.textCtrlSetStyle highlighter from to attr
