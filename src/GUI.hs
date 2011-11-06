-- module GUI where

import qualified IO
import Term
import Program ( Program, modules )
import qualified Program
import qualified Module
import qualified Controls
import qualified Rewrite
import qualified Option
import qualified Log
import Utility ( void )

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Controls
import Graphics.UI.WX.Events
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Types
           ( Color, rgb, fontFixed, Point2(Point), sz,
             varCreate, varSwap, varUpdate )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Control.Monad.STM as STM

import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef )

import qualified Graphics.UI.WXCore as WXCore
import qualified Graphics.UI.WXCore.WxcClassesAL as WXCAL
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )

import Graphics.UI.WXCore.Events
import Event

import qualified ALSA
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( forever, forM_ )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Pos as Pos

import Control.Exception ( bracket, finally )
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Maybe ( maybeToList, fromMaybe )

import qualified Data.List as List
import Data.Tuple.HT ( fst3 )

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    IO.hSetBuffering IO.stderr IO.LineBuffering
    opt <- Option.get

    (p,ctrls) <-
        Exc.resolveT (\e -> IO.hPutStrLn IO.stderr (show e) >> Exit.exitFailure) $
            prepareProgram =<<
            Program.chase (Option.importPaths opt) ( Option.moduleName opt )

    input <- newChan
    output <- newChan
    writeChan output $ Register p ctrls
    ALSA.withSequencer "Rewrite-Sequencer" $ \sq -> do
        ALSA.parseAndConnect sq ( Option.connectTo opt )
        flip finally (ALSA.stopQueue sq) $ WX.start $ do
            gui input output
            void $ forkIO $ machine input output (Option.importPaths opt) p sq

-- | messages that are sent from GUI to machine
data Action =
     Modification Identifier String Int -- ^ modulename, sourcetext, position
   | Execution Execution
   | Control Controls.Event
   | Load FilePath

data Execution = Restart | Stop | Pause | Continue


data ExceptionType = ParseException | TermException
    deriving (Show, Eq, Ord, Enum)

-- | messages that are sent from machine to GUI
data GuiUpdate =
     Term { _steps :: [ Rewrite.Message ], _currentTerm :: String }
   | Exception { _excType :: ExceptionType, _range :: Range, _message :: String }
   | Register Program [(Identifier, Controls.Control)]
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

exceptionToGUI ::
    Chan GuiUpdate ->
    Exc.ExceptionalT (Range, String) IO () ->
    IO ()
exceptionToGUI output =
    Exc.resolveT
        (writeChan output .
         uncurry (Exception ParseException))

prepareProgram ::
    (Monad m) =>
    Program ->
    Exc.ExceptionalT (Range, String) m
        (Program, [(Identifier, Controls.Control)])
prepareProgram p0 = do
    let ctrls = Controls.collect p0
    p1 <- Exc.ExceptionalT $ return $
        Program.add_module (Controls.controller_module ctrls) p0
    return (p1, ctrls)

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
        -> [FilePath]
        -> Program -- ^ initial program
        -> ALSA.Sequencer SndSeq.DuplexMode
        -> IO ()
machine input output importPaths progInit sq = do
    program <- newTVarIO progInit
    let mainName = read "main"
    term <- newTMVarIO mainName

    void $ forkIO $ flip MS.evalStateT mainName $ forever $ do
        action <- liftIO $ readChan input
        let running =
                liftIO . writeChan output . Running
        case action of
            Control event -> liftIO $ do
                Log.put $ show event
                exceptionToGUI output $
                    Exc.mapExceptionalT STM.atomically $ do
                        p <- lift $ readTVar program
                        p' <-
                            Exc.ExceptionalT $ return $
                            Controls.change_controller_module p event
                        lift $ writeTVar program p'
                        -- return $ Controls.get_controller_module p'
                -- Log.put $ show m
            Execution exec ->
                case exec of
                    Restart -> do
                        running True
                        MS.put mainName
                        liftIO $ ALSA.quietContinueQueue sq
                        liftIO $ void $ STM.atomically $ writeTMVar term mainName
                    Stop -> do
                        running False
                        liftIO $ ALSA.stopQueue sq
                        liftIO $ void $ STM.atomically $ tryTakeTMVar term
                        MS.put mainName
                    Pause -> do
                        running False
                        liftIO $ ALSA.pauseQueue sq
                        MS.put .
                            maybe mainName id
                            =<< (liftIO $ STM.atomically $ tryTakeTMVar term)
                    Continue -> do
                        running True
                        liftIO $ ALSA.continueQueue sq
                        liftIO . STM.atomically . writeTMVar term
                            =<< MS.get
                        MS.put mainName
            Modification moduleName sourceCode pos -> liftIO $ do
                Log.put $
                    "module " ++ show moduleName ++
                    " has new input\n" ++ sourceCode
                exceptionToGUI output $ do
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
                    lift $ Log.put "parsed and modified OK"
            Load filePath -> liftIO $ do
                Log.put $
                    "load " ++ filePath ++ " and all its depenencies"
                exceptionToGUI output $ do
                    (p,ctrls) <-
                        prepareProgram =<<
                        Program.load importPaths Program.empty
                            (FilePath.takeBaseName filePath) filePath
                    lift $ do
                        ALSA.stopQueue sq
                        STM.atomically $ do
                            writeTVar program p
                            writeTMVar term mainName
                        ALSA.continueQueue sq
                        writeChan output $ Register p ctrls
                        Log.put "chased and parsed OK"

    ALSA.startQueue sq
    MS.evalStateT
        ( execute program term ( writeChan output ) sq ) 0


execute :: TVar Program
                  -- ^ current program (GUI might change the contents)
        -> TMVar Term -- ^ current term
        -> ( GuiUpdate -> IO () ) -- ^ sink for messages (show current term)
        -> ALSA.Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> MS.StateT Time IO ()
execute program term output sq = forever $ do
    -- Log.put "execute"
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
            ALSA.stopQueue sq
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
myEventId = wxID_HIGHEST+100
    -- the custom event ID, avoid clash with Graphics.UI.WXCore.Types.varTopId

-- | the custom event is registered as a menu event
createMyEvent :: IO (WXCore.CommandEvent ())
createMyEvent =
    WXCAL.commandEventCreate WXCMZ.wxEVT_COMMAND_MENU_SELECTED myEventId

registerMyEvent :: WXCore.EvtHandler a -> IO () -> IO ()
registerMyEvent win io = evtHandlerOnMenuCommand win myEventId io


-- might be moved to wx package
cursor :: WX.Attr (TextCtrl a) Int
cursor =
    WX.newAttr "cursor"
        WXCMZ.textCtrlGetInsertionPoint
        WXCMZ.textCtrlSetInsertionPoint

editable :: WX.Attr (TextCtrl a) Bool
editable =
    WX.newAttr "editable"
        WXCMZ.textCtrlIsEditable
        WXCMZ.textCtrlSetEditable

_modified :: WX.ReadAttr (TextCtrl a) Bool
_modified =
    WX.readAttr "modified"
        WXCMZ.textCtrlIsModified
--        WXCMZ.textCtrlDiscardEdits
--        WXCMZ.textCtrlMarkDirty


notebookSelection :: WX.Attr (Notebook a) Int
notebookSelection =
    WX.newAttr "selection"
        WXCMZ.notebookGetSelection
        (\nb -> void . WXCMZ.notebookSetSelection nb)



{-
The order of widget creation is important
for cycling through widgets using tabulator key.
-}
gui :: Chan Action -- ^  the gui writes here
      -- (if the program text changes due to an edit action)
    -> Chan GuiUpdate -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> IO ()
gui input output = do
    program <- newIORef Program.empty
    controls <- newIORef []
    panels <- newIORef M.empty

    let highlighters = fmap ( \ (_pnl,_,h) -> h )
        editors = fmap ( \ (_pnl,e,_) -> e )


    frameError <- WX.frame [ text := "errors" ]

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

    f <- WX.frame
        [ text := "live-sequencer", visible := False
        ]

    out <- newChan

    void $ forkIO $ forever $ do
        writeChan out =<< readChan output
        WXCAL.evtHandlerAddPendingEvent f =<< createMyEvent

    p <- WX.panel f [ ]


    fileMenu <- WX.menuPane [text := "&File"]

    let haskellFilenames =
            [ ("Haskell modules", ["*.hs"]),
              ("All files", ["*"]) ]

    loadItem <- WX.menuItem fileMenu
        [ text := "L&oad ...\tCtrl-O",
          help :=
              "flush all modules " ++
              "and load a new program with all its dependencies" ]
    saveItem <- WX.menuItem fileMenu
        [ text := "&Save\tCtrl-S",
          help :=
              "overwrite original file with current module content" ]
    saveAsItem <- WX.menuItem fileMenu
        [ text := "Save as ...",
          help :=
              "save module content to a different or new file " ++
              "and make this the new file target" ]

    WX.menuLine fileMenu

    quitItem <- WX.menuQuit fileMenu []


    execMenu <- WX.menuPane [text := "&Execution"]

    refreshItem <- WX.menuItem execMenu
        [ text := "&Refresh\tCtrl-R",
          help :=
              "parse the edited program and if successful " ++
              "replace the executed program" ]
    WX.menuLine execMenu
    _restartItem <- WX.menuItem execMenu
        [ text := "Res&tart\tCtrl-T",
          on command := writeChan input (Execution Restart),
          help :=
              "stop sound and restart program execution with 'main'" ]
    _stopItem <- WX.menuItem execMenu
        [ text := "Stop\tCtrl-Z",
          on command := writeChan input (Execution Stop),
          help :=
              "stop program execution and sound, " ++
              "reset term to 'main'" ]
    runningItem <- WX.menuItem execMenu
        [ text := "running\tCtrl-U",
          checkable := True,
          checked := True,
          help := "pause or continue program execution" ]


    windowMenu <- WX.menuPane [text := "&Window"]

    appRunning <- newIORef True
    let windowMenuItem title win = do
            itm <- WX.menuItem windowMenu
                [ text := title,
                  help := "show or hide " ++ title ++ " window",
                  checkable := True,
                  checked := True ]
            set itm
                [ on command := do
                    b <- get itm checked
                    set win [ visible := b ] ]
            set win
                [ on closing := do
                    run <- readIORef appRunning
                    if run
                      then do
                        set itm [ checked := False ]
                        set win [ visible := False ]
                        -- WXCMZ.closeEventVeto ??? True
                      else propagateEvent ]

    windowMenuItem "errors" frameError
    windowMenuItem "controls" frameControls


    nb <- WX.notebook p [ ]


    reducer <-
        textCtrl p
            [ font := fontFixed, editable := False, wrap := WrapNone ]

    status <- WX.statusField
        [ text := "Welcome to interactive music composition with Haskell" ]


    set loadItem [
          on command := do
              mfilename <- WX.fileOpenDialog
                  f False {- change current directory -} True
                  "Load Haskell program" haskellFilenames "" ""
              case mfilename of
                  Nothing -> return ()
                  Just filename ->
                      writeChan input $ Load filename ]

    let getCurrentModule = do
            index <- get nb notebookSelection
            (moduleName, (_panel, editor, _highlighter)) <-
                fmap ( M.elemAt index ) $ readIORef panels
            content <- get editor text
            prg <- readIORef program
            return
                (Module.source_location $ snd $
                 M.elemAt index (modules prg),
                 moduleName, content)
        saveModule (path, moduleName, content) = do
            -- Log.put path
            writeFile path content
            set status [
                text := "module " ++ show moduleName ++ " saved to " ++ path ]

    set saveItem [
          on command := do
              saveModule =<< getCurrentModule ]

    set saveAsItem [
          on command := do
              (filePath, moduleName, content) <- getCurrentModule
              let (path,file) = FilePath.splitFileName filePath
              -- print (path,file)
              mfilename <- WX.fileSaveDialog
                  f False {- change current directory -} True
                  ("Save module " ++ show moduleName) haskellFilenames path file
              case mfilename of
                  Nothing -> return ()
                  Just fileName -> do
                      saveModule (fileName, moduleName, content)
                      modifyIORef program $ \prg ->
                          prg { Program.modules =
                              M.adjust
                                  (\modu -> modu { Module.source_location = fileName })
                                  moduleName (Program.modules prg) }
          ]


    let refreshProgram (moduleName, (_panel, editor, _highlighter)) = do
            s <- get editor text
            pos <- get editor cursor
            writeChan input $ Modification moduleName s pos

            modifyIORef errorList
                (Seq.filter
                    (\(_, errorRng, _) ->
                        name moduleName /= Pos.sourceName (Term.start errorRng)))
            refreshErrorLog

    set refreshItem
        [ on command := do
            refreshProgram =<< getFromNotebook nb =<< readIORef panels
            -- mapM_ refreshProgram pnls
            ]

    set runningItem
        [ on command := do
            running <- get runningItem checked
            writeChan input . Execution $
                if running then Continue else Pause ]


    set f [
            layout := container p $ margin 5
            $ column 5
            [ WX.fill $ tabs nb []
            , WX.fill $ widget reducer
            ]
            , WX.statusBar := [status]
            , WX.menuBar   := [fileMenu, execMenu, windowMenu]
            , visible := True
            , clientSize := sz 500 300
          ]


    set errorLog
        [ on listEvent := \ev ->
              case ev of
                  ListItemSelected n -> do
                      errors <- readIORef errorList
                      let (typ, errorRng, _descr) = Seq.index errors n
                      case Parsec.parse IO.input "" $
                           Pos.sourceName $ Term.start errorRng of
                          Left _ -> return ()
                          Right moduleIdent -> do
                              pnls <- readIORef panels
                              set nb [ notebookSelection :=
                                           M.findIndex moduleIdent pnls ]
                              let textField =
                                      case typ of
                                          ParseException -> editors pnls
                                          TermException -> highlighters pnls
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
        , clientSize := sz 500 300
        ]

    let closeOther =
            writeIORef appRunning False >>
            close frameError >> close frameControls
    set quitItem [ on command := closeOther >> close f]
    set f [ on closing := closeOther >> propagateEvent
        {- 'close f' would trigger the closing handler again -} ]
    focusOn f


    highlights <- varCreate M.empty

    registerMyEvent f $ do
        msg <- readChan out
        let setColorHighlighters ::
                M.Map Identifier [Identifier] -> Int -> Int -> Int -> IO ()
            setColorHighlighters m r g b = do
                pnls <- readIORef panels
                set_color nb ( highlighters pnls ) m ( rgb r g b )
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
            Refresh moduleName s pos -> do
                pnls <- readIORef panels
                maybe (return ())
                    (\h -> set h [ text := s, cursor := pos ])
                    (M.lookup moduleName $ highlighters pnls)
                set status [ text :=
                    "module " ++ show moduleName ++ " reloaded into interpreter" ]

            Register prg ctrls -> do
                writeIORef program prg
                writeIORef controls ctrls

                void $ WXCMZ.notebookDeleteAllPages nb
                pnls <- displayModules input
                            frameControls ctrls nb prg
                writeIORef panels pnls
                Fold.forM_ (M.mapWithKey (,) $ fmap fst3 pnls) $ \(moduleName,sub) ->
                    WXCMZ.notebookAddPage nb sub (show moduleName) False (-1)
                set status [ text :=
                    "modules loaded: " ++
                    (List.intercalate ", " $ map show $
                     M.keys $ Program.modules prg) ]

            ResetDisplay -> do
                previous <- varSwap highlights M.empty
                setColorHighlighters previous 255 255 255

            Running running -> do
                set status [ text :=
                    if running
                      then "interpreter started"
                      else "interpreter stopped" ]
                set runningItem [ checked := running ]

displayModules ::
    Chan Action ->
    WX.Frame c ->
    [(Identifier, Controls.Control)] ->
    WXCore.Window b ->
    Program ->
    IO (M.Map Identifier (Panel (), TextCtrl (), TextCtrl ()))
displayModules input frameControls ctrls nb prog = do
    Controls.create frameControls ctrls
        $ \ e -> writeChan input ( Control e )

    Trav.forM (modules prog) $ \ content -> do
        psub <- panel nb []
        editor <- textCtrl psub [ font := fontFixed, wrap := WrapNone ]
        highlighter <- textCtrlRich psub
            [ font := fontFixed, wrap := WrapNone, editable := False ]
        set editor [ text := Module.source_text content ]
        set highlighter [ text := Module.source_text content ]
        set psub [ layout := (row 5 $
            map WX.fill $ [widget editor, widget highlighter]) ]
        return (psub, editor, highlighter)


getFromNotebook ::
    Notebook b -> M.Map k a -> IO (k, a)
getFromNotebook nb m =
    fmap (flip M.elemAt m) $ get nb notebookSelection

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
    (p, highlighter) <- getFromNotebook nb highlighters
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
