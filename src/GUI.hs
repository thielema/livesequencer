-- module GUI where

import qualified IO
import qualified Term
import qualified Program
import qualified Exception
import qualified Module
import qualified Controls
import qualified Rewrite
import qualified Option
import qualified Log
import Program ( Program, modules )
import Term ( Term, Identifier )
import Utility ( void )

import qualified HTTPServer

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
import Control.Concurrent.MVar
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
import qualified Event

import Foreign.Storable ( peek )
import Foreign.Marshal.Alloc ( alloca )

import qualified ALSA
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.MIDI.Message.Channel.Voice as VM

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( liftM2, forever, forM_ )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Exception ( bracket, finally, try )
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath

import qualified Data.Accessor.Monad.Trans.State as AccM
import qualified Data.Accessor.Tuple as AccTuple

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Maybe ( maybeToList, fromMaybe )

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Tuple.HT ( fst3 )

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    IO.hSetBuffering IO.stderr IO.LineBuffering
    opt <- Option.get

    (p,ctrls) <-
        Exc.resolveT
            (\e ->
                IO.hPutStrLn IO.stderr (Exception.multilineFromMessage e) >>
                Exit.exitFailure) $
            prepareProgram =<<
            Program.chase (Option.importPaths opt) (Option.moduleName opt)

    input <- newChan
    output <- newChan
    writeChan output $ Register p ctrls
    ALSA.withSequencer "Rewrite-Sequencer" $ \sq -> do
        ALSA.parseAndConnect sq
            ( Option.connectFrom opt ) ( Option.connectTo opt )
        flip finally (ALSA.stopQueue sq) $ WX.start $ do
            gui input output
            void $ forkIO $ machine input output (Option.importPaths opt) p sq
            void $ forkIO $ HTTPServer.run (httpMethods output) (Option.httpPort opt)

type HTTPFeedback = Exc.Exceptional HTTPServer.Error (Maybe String, String)

-- | messages that are sent from GUI to machine
data Action =
     Modification (Maybe (MVar HTTPFeedback)) Identifier String Int
         -- ^ MVar of the HTTP server, modulename, sourcetext, position
   | Execution Execution
   | NextStep
   | Control Controls.Event
   | Load FilePath

data Execution =
    Mode Event.WaitMode | Restart | Stop | PlayTerm String


-- | messages that are sent from machine to GUI
data GuiUpdate =
     Term { _steps :: [ Rewrite.Message ], _currentTerm :: String }
   | Exception { _message :: Exception.Message }
   | Register Program [(Identifier, Controls.Control)]
   | Refresh { _moduleName :: Identifier, _content :: String, _position :: Int }
   | InsertText { _insertedText :: String }
   | GetModuleList { _moduleList :: MVar [ Identifier ] }
   | GetModuleContent {
         _moduleName :: Identifier,
         _moduleContent :: MVar (Exc.Exceptional HTTPServer.Error String) }
   | UpdateModuleContent {
         _moduleName :: Identifier,
         _moduleEditableContent :: String,
         _moduleNewContent :: MVar HTTPFeedback }
   | Running { _runningMode :: Event.WaitMode }
   | ResetDisplay


exceptionToGUI ::
    Chan GuiUpdate ->
    Exc.ExceptionalT Exception.Message IO () ->
    IO ()
exceptionToGUI output =
    Exc.resolveT (writeChan output . Exception)

prepareProgram ::
    (Monad m) =>
    Program ->
    Exc.ExceptionalT Exception.Message m
        (Program, [(Identifier, Controls.Control)])
prepareProgram p0 = do
    let ctrls = Controls.collect p0
    p1 <- Exc.ExceptionalT $ return $
        Program.add_module (Controls.controller_module ctrls) p0
    return (p1, ctrls)

formatPitch :: VM.Pitch -> String
formatPitch p =
    let (oct,cls) = divMod (VM.fromPitch p) 12
        name =
            case cls of
                00 -> "c"
                01 -> "cs"
                02 -> "d"
                03 -> "ds"
                04 -> "e"
                05 -> "f"
                06 -> "fs"
                07 -> "g"
                08 -> "gs"
                09 -> "a"
                10 -> "as"
                11 -> "b"
                _ -> error "pitch class must be a number from 0 to 11"
    in  "note qn (" ++ name ++ " " ++ show (oct-1) ++ ") : "


modifyModule ::
    TVar Program ->
    Chan GuiUpdate ->
    Identifier ->
    [Char] ->
    Int ->
    Exc.ExceptionalT Exception.Message IO ()
modifyModule program output moduleName sourceCode pos = do
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
    lift $ writeChan output $
        Refresh moduleName sourceCode pos
    lift $ Log.put "parsed and modified OK"



writeTMVar :: TMVar a -> a -> STM.STM ()
writeTMVar var a =
    clearTMVar var >> putTMVar var a

clearTMVar :: TMVar a -> STM.STM ()
clearTMVar var =
    void $ tryTakeTMVar var

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
    waitChan <- newChan

    void $ forkIO $ forever $ do
        action <- readChan input
        let withMode mode transaction = do
                writeChan output $ Running mode
                writeChan waitChan $ Event.ModeChange mode
                STM.atomically transaction
        case action of
            Control event -> do
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
            NextStep -> writeChan waitChan Event.NextStep
            Execution exec ->
                case exec of
                    Mode mode -> do
                        case mode of
                            Event.RealTime -> do
                                ALSA.continueQueue sq
                            Event.SlowMotion _ -> do
                                ALSA.continueQueue sq
                            Event.SingleStep -> do
                                ALSA.pauseQueue sq
                        withMode mode $ return ()
                    Restart -> do
                        ALSA.quietContinueQueue sq
                        withMode Event.RealTime $ writeTMVar term mainName
                    Stop -> do
                        ALSA.stopQueue sq
                        withMode Event.SingleStep $ writeTMVar term mainName
                    PlayTerm str ->
                        case Parsec.parse
                                 (Parsec.between
                                     (Token.whiteSpace Term.lexer)
                                     (Parsec.eof)
                                     IO.input)
                                 "marked range" str of
                            Left msg ->
                                writeChan output . Exception $
                                Program.messageFromParserError msg
                            Right t -> do
                                ALSA.quietContinueQueue sq
                                withMode Event.RealTime $ writeTMVar term t
            Modification feedback moduleName sourceCode pos -> do
                Log.put $
                    "module " ++ show moduleName ++
                    " has new input\n" ++ sourceCode
                case feedback of
                    Nothing ->
                        exceptionToGUI output $
                            modifyModule program output moduleName sourceCode pos
                    Just mvar -> do
                        result <-
                            Exc.runExceptionalT $
                            modifyModule program output moduleName sourceCode pos
                        case result of
                            Exc.Success () ->
                                putMVar mvar $ Exc.Success (Nothing, sourceCode)
                            Exc.Exception e -> do
                                writeChan output $ Exception e
                                putMVar mvar $ Exc.Success $
                                    (Just $ Exception.multilineFromMessage e,
                                     sourceCode)
            Load filePath -> do
                Log.put $
                    "load " ++ filePath ++ " and all its dependencies"
                exceptionToGUI output $ do
                    (p,ctrls) <-
                        prepareProgram =<<
                        Program.load importPaths Program.empty
                            (FilePath.takeBaseName filePath) filePath
                    lift $ do
                        ALSA.stopQueue sq
                        withMode Event.RealTime $ do
                            writeTVar program p
                            writeTMVar term mainName
                        ALSA.continueQueue sq
                        writeChan output $ Register p ctrls
                        Log.put "chased and parsed OK"

    void $ forkIO $
        Event.listen sq
            ( writeChan output . InsertText . formatPitch ) waitChan
    ALSA.startQueue sq
    flip MS.evalStateT (Event.RealTime, 0) $
        execute program term ( writeChan output ) sq waitChan


execute :: TVar Program
                  -- ^ current program (GUI might change the contents)
        -> TMVar Term -- ^ current term
        -> ( GuiUpdate -> IO () ) -- ^ sink for messages (show current term)
        -> ALSA.Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> Chan Event.WaitResult
        -> MS.StateT Event.State IO ()
execute program term output sq waitChan = forever $ do
    let mainName = read "main"
    ( ( es, log ), result ) <- liftIO $ STM.atomically $ do
        t <- takeTMVar term
        p <- readTVar program
            {- this happens anew at each click
               since the program text might have changed in the editor -}
        let eslog@( es, _log ) =
                flip Rewrite.runEval p $
                Rewrite.force_head t
        let returnExc pos =
                return . Exc.Exception . (,) pos
        fmap ((,) eslog) $
            case es of
                Exc.Exception (pos,msg) -> returnExc pos msg
                Exc.Success s ->
                    case Term.viewNode s of
                        Just (":", [x, xs]) -> do
                            putTMVar term xs
                            return $ Exc.Success x
                        Just ("[]", []) -> do
                            putTMVar term mainName
                            returnExc (Term.termRange s) "finished."
                        _ -> do
                            putTMVar term mainName
                            returnExc (Term.termRange s) $
                                "I do not know how to handle this term: " ++ show s

    liftIO $ Exc.switch (const $ return ()) (output . Term log . show) es

    case result of
        Exc.Exception (rng,msg) -> do
            liftIO $ do
                ALSA.stopQueue sq
                -- writeChan waitChan $ Event.ModeChange Event.SingleStep
                output $ Exception $ Exception.Message Exception.Term rng msg
                output $ Running Event.SingleStep
            {-
            We have to alter the mode directly,
            since the channel is only read when we wait for a duration other than Nothing
            -}
            AccM.set AccTuple.first Event.SingleStep
            Event.wait sq waitChan Nothing
        Exc.Success x -> do
            Exc.resolveT
                (liftIO . output . Exception)
                (Event.play sq waitChan x)
            case Term.viewNode x of
                Just ("Wait", _) -> liftIO $ output ResetDisplay
                _ -> return ()


httpMethods :: Chan GuiUpdate -> HTTPServer.Methods
httpMethods output = HTTPServer.Methods {
        HTTPServer.getModuleList = do
            modList <- newEmptyMVar
            writeChan output $ GetModuleList modList
            takeMVar modList,
        HTTPServer.getModuleContent = \name -> Exc.ExceptionalT $ do
            content <- newEmptyMVar
            writeChan output $ GetModuleContent name content
            takeMVar content,
        HTTPServer.updateModuleContent = \name edited -> Exc.ExceptionalT $ do
            newContent <- newEmptyMVar
            writeChan output $ UpdateModuleContent name edited newContent
            takeMVar newContent
    }


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
    let updateErrorLog f = do
            errors <- readIORef errorList
            let newErrors = f errors
            writeIORef errorList newErrors
            set errorLog [ items :=
                  map Exception.lineFromMessage $ Fold.toList newErrors ]

    clearLog <- WX.button panelError
        [ text := "Clear",
          on command := updateErrorLog (const Seq.empty) ]

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
        [ text := "L&oad and check program ...\tCtrl-O",
          help :=
              "flush all modules " ++
              "and load a new program with all its dependencies" ]
    reloadItem <- WX.menuItem fileMenu
        [ text := "&Reload module",
          help :=
              "reload a module from its original file, " ++
              "but do not pass it to the interpreter" ]
    saveItem <- WX.menuItem fileMenu
        [ text := "&Save module\tCtrl-S",
          help :=
              "overwrite original file with current module content" ]
    saveAsItem <- WX.menuItem fileMenu
        [ text := "Save module &as ...",
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
    realTimeItem <- WX.menuItem execMenu
        [ text := "Real time",
          checkable := True,
          checked := True,
          help := "pause according to Wait elements" ]
    slowMotionItem <- WX.menuItem execMenu
        [ text := "Slow motion",
          checkable := True,
          help := "pause between every list element" ]
    singleStepItem <- WX.menuItem execMenu
        [ text := "Single step",
          checkable := True,
          help := "wait for user confirmation after every list element" ]
    WX.menuLine execMenu
    _restartItem <- WX.menuItem execMenu
        [ text := "Res&tart\tCtrl-T",
          on command := writeChan input (Execution Restart),
          help :=
              "stop sound and restart program execution with 'main'" ]
    playTermItem <- WX.menuItem execMenu
        [ text := "Play term\tCtrl-M",
          help :=
              "stop sound and restart program execution " ++
              "with the marked editor area as current term, " ++
              "or use the surrounding identifier if nothing is marked" ]
    _stopItem <- WX.menuItem execMenu
        [ text := "Stop\tCtrl-Space",
          on command := writeChan input (Execution Stop),
          help :=
              "stop program execution and sound, " ++
              "reset term to 'main'" ]

    WX.menuLine execMenu

    fasterItem <- WX.menuItem execMenu
        [ text := "Faster\tCtrl->",
          enabled := False,
          help := "decrease pause in slow motion mode" ]
    slowerItem <- WX.menuItem execMenu
        [ text := "Slower\tCtrl-<",
          enabled := False,
          help := "increase pause in slow motion mode" ]
    nextStepItem <- WX.menuItem execMenu
        [ text := "Next step\tCtrl-N",
          enabled := False,
          on command := writeChan input NextStep,
          help := "perform next step in single step mode" ]


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


    let handleException moduleName act = do
            result <- try act
            case result of
                Left err ->
                    writeChan output $
                    Exception $ Exception.Message Exception.InOut
                        (Program.dummyRange (show moduleName))
                        (Err.ioeGetErrorString err)
                Right () -> return ()

    set loadItem [
          on command := do
              mfilename <- WX.fileOpenDialog
                  f False {- change current directory -} True
                  "Load Haskell program" haskellFilenames "" ""
              case mfilename of
                  Nothing -> return ()
                  Just filename ->
                      writeChan input $ Load filename ]

    set reloadItem [
          on command := do
              index <- get nb notebookSelection
              (moduleName, (_panel, editor, _highlighter)) <-
                  fmap ( M.elemAt index ) $ readIORef panels
              prg <- readIORef program
              let path =
                      Module.source_location $ snd $
                      M.elemAt index (modules prg)

              handleException moduleName $ do
                  content <- readFile path
                  set editor [ text := content ]
                  set status [
                      text := "module " ++ show moduleName ++ " reloaded from " ++ path ]
          ]

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
        saveModule (path, moduleName, content) =
            handleException moduleName $ do
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
            writeChan input $ Modification Nothing moduleName s pos

            updateErrorLog $ Seq.filter $
                \(Exception.Message _ errorRng _) ->
                    Term.name moduleName /= Pos.sourceName (Term.start errorRng)

    set refreshItem
        [ on command := do
            refreshProgram =<< getFromNotebook nb =<< readIORef panels
            -- mapM_ refreshProgram pnls
            ]

    set playTermItem
        [ on command := do
            (_moduleName, (_panel, editor, _highlighter)) <-
                getFromNotebook nb =<< readIORef panels
            marked <- WXCMZ.textCtrlGetStringSelection editor
            expr <-
                if null marked
                  then do
                      (i,line) <-
                          textColumnRowFromPos editor
                              =<< get editor cursor
                      content <- WXCMZ.textCtrlGetLineText editor line
{- simpler but inefficient
                      content <- get editor text
                      i <- get editor cursor
-}
                      case splitAt i content of
                          (prefix,suffix) ->
                              let identLetter c = Char.isAlphaNum c || c == '_' || c == '.'
                              in  return $
                                      (reverse $ takeWhile identLetter $ reverse prefix)
                                      ++
                                      takeWhile identLetter suffix
                  else return marked
            writeChan input . Execution . PlayTerm $ expr ]

    waitDuration <- newIORef 500

    let updateSlowMotionDur = do
            dur <- readIORef waitDuration
            writeChan input $ Execution $ Mode $ Event.SlowMotion dur

    set fasterItem [
        on command := do
            modifyIORef waitDuration (\d -> max 100 (d-100))
            updateSlowMotionDur
            d <- readIORef waitDuration
            set status [ text :=
                "decreased pause to " ++ show d ++ "ms" ] ]

    set slowerItem [
        on command := do
            modifyIORef waitDuration (100+)
            updateSlowMotionDur
            d <- readIORef waitDuration
            set status [ text :=
                "increased pause to " ++ show d ++ "ms" ] ]

    let setRealTime b = do
            set realTimeItem [ checked := b ]

        setSlowMotion b = do
            set slowMotionItem [ checked := b ]
            set fasterItem [ enabled := b ]
            set slowerItem [ enabled := b ]

        setSingleStep b = do
            set singleStepItem [ checked := b ]
            set nextStepItem [ enabled := b ]

        onActivation w act =
            set w [ on command := do
                b <- get w checked
                if b then act else set w [checked := True] ]

        activateRealTime = do
            setRealTime True
            setSlowMotion False
            setSingleStep False

        activateSlowMotion = do
            setRealTime False
            setSlowMotion True
            setSingleStep False

        activateSingleStep = do
            setRealTime False
            setSlowMotion False
            setSingleStep True

    onActivation realTimeItem $ do
        activateRealTime
        writeChan input $ Execution $ Mode Event.RealTime
    onActivation slowMotionItem $ do
        activateSlowMotion
        updateSlowMotionDur
    onActivation singleStepItem $ do
        activateSingleStep
        writeChan input $ Execution $ Mode Event.SingleStep


    set f [
            layout := container p $ margin 5
            $ column 5
            [ WX.fill $ tabs nb []
            , WX.fill $ widget reducer
            ]
            , WX.statusBar := [status]
            , WX.menuBar   := [fileMenu, execMenu, windowMenu]
            , visible := True
            , clientSize := sz 1280 720
          ]


    set errorLog
        [ on listEvent := \ev -> void $ MaybeT.runMaybeT $ do
              ListItemSelected n <- return ev
              errors <- liftIO $ readIORef errorList
              let (Exception.Message typ errorRng _descr) =
                      Seq.index errors n
              Right moduleIdent <- return $
                  Parsec.parse IO.input "" $
                  Pos.sourceName $ Term.start errorRng
              pnls <- liftIO $ readIORef panels
              pnl <- MaybeT.MaybeT $ return $ M.lookupIndex moduleIdent pnls
              liftIO $ set nb [ notebookSelection := pnl ]
              let activateText textField = do
                      h <- MaybeT.MaybeT $ return $
                           M.lookup moduleIdent textField
                      i <- liftIO $ textPosFromSourcePos h $ Term.start errorRng
                      j <- liftIO $ textPosFromSourcePos h $ Term.end errorRng
                      liftIO $ set h [ cursor := i ]
                      liftIO $ WXCMZ.textCtrlSetSelection h i j
              case typ of
                  Exception.Parse ->
                      activateText $ editors pnls
                  Exception.Term ->
                      activateText $ highlighters pnls
                  Exception.InOut ->
                      return ()
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

            Exception exc -> do
                itemAppend errorLog $ Exception.lineFromMessage exc
                modifyIORef errorList (Seq.|> exc)
                set status [ text := Exception.statusFromMessage exc ]

            -- update highlighter text field only if parsing was successful
            Refresh moduleName s pos -> do
                pnls <- readIORef panels
                maybe (return ())
                    (\h -> set h [ text := s, cursor := pos ])
                    (M.lookup moduleName $ highlighters pnls)
                set status [ text :=
                    "module " ++ show moduleName ++ " reloaded into interpreter" ]
            InsertText str -> do
                (_moduleName, (_panel, editor, _highlighter)) <-
                    getFromNotebook nb =<< readIORef panels
                WXCMZ.textCtrlWriteText editor str
                set status [ text :=
                    "inserted note from external controller" ]

            Register prg ctrls -> do
                writeIORef program prg
                writeIORef controls ctrls

                void $ WXCMZ.notebookDeleteAllPages nb
                pnls <- displayModules input
                            frameControls ctrls nb prg
                writeIORef panels pnls
                Fold.forM_ (M.mapWithKey (,) $ fmap fst3 pnls) $
                    \(moduleName,sub) ->
                        WXCMZ.notebookAddPage nb sub (show moduleName) False (-1)

                updateErrorLog (const Seq.empty)

                set status [ text :=
                    "modules loaded: " ++
                    (List.intercalate ", " $ map show $
                     M.keys $ Program.modules prg) ]

            ResetDisplay -> do
                previous <- varSwap highlights M.empty
                setColorHighlighters previous 255 255 255

            Running mode -> do
                case mode of
                    Event.RealTime -> do
                        set status [ text := "interpreter in real-time mode" ]
                        activateRealTime
                    Event.SlowMotion dur -> do
                        set status [ text :=
                            ("interpreter in slow-motion mode with pause " ++
                             show dur ++ "ms") ]
                        activateSlowMotion
                    Event.SingleStep -> do
                        set status [ text :=
                            "interpreter in single step mode," ++
                            " waiting for next step" ]
                        activateSingleStep

            GetModuleList modList ->
                putMVar modList . M.keys =<< readIORef panels

            GetModuleContent name content ->
                (putMVar content =<<) $ Exc.runExceptionalT $ do
                    pnls <- lift $ readIORef panels
                    (_,editor,_) <- getModuleForHTTP pnls name
                    lift $ set status [ text :=
                        "module " ++ show name ++ " downloaded by web client" ]
                    lift $ get editor text

            UpdateModuleContent name content contentMVar -> do
                result <- Exc.runExceptionalT $ do
                    pnls <- lift $ readIORef panels
                    (_,editor,_) <-
                        case M.lookup name pnls of
                            Nothing ->
                                Exc.throwT
                                    ("Module " ++ show name ++ " no longer available.",
                                     "")
                            Just pnl -> return pnl
                    lift $ set status [ text :=
                        "module " ++ show name ++ " updated by web client" ]
                    pos <- lift $ get editor cursor
                    localContent <- lift $ get editor text
                    case HTTPServer.splitProtected localContent of
                        (protected, sepEditable) ->
                            case sepEditable of
                                Nothing ->
                                    Exc.throwT
                                        ("Module does no longer contain a separation mark, " ++
                                         "thus you cannot alter the content.",
                                         protected)
                                Just (sep, _edit) -> do
                                    let newContent = protected ++ sep ++ '\n' : content
                                    lift $ set editor [ text := newContent, cursor := pos ]
                                    return (newContent, pos)
                case result of
                    Exc.Exception (e, protected) ->
                        putMVar contentMVar $
                            Exc.Success (Just e,
                                protected ++ HTTPServer.separatorLine ++ '\n' : content)
                    Exc.Success (newContent, pos) ->
                        writeChan input $
                        Modification (Just contentMVar) name newContent pos

getModuleForHTTP ::
    (Monad m) =>
    M.Map Identifier a ->
    Identifier ->
    Exc.ExceptionalT HTTPServer.Error m a
getModuleForHTTP pnls name =
    Exc.ExceptionalT $ return $
    Exc.fromMaybe
        (HTTPServer.notFound $ "module " ++ show name ++ " not found") $
    M.lookup name pnls

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
textPosFromSourcePos textArea pos =
    WXCMZ.textCtrlXYToPosition textArea
       $ Point (Pos.sourceColumn pos - 1)
               (Pos.sourceLine   pos - 1)

textColumnRowFromPos ::
    TextCtrl a -> Int -> IO (Int, Int)
textColumnRowFromPos textArea pos =
    alloca $ \rowPtr ->
    alloca $ \columnPtr -> do
        void $ WXCMZ.textCtrlPositionToXY textArea pos columnPtr rowPtr
        liftM2 (,)
            (fmap fromIntegral $ peek columnPtr)
            (fmap fromIntegral $ peek rowPtr)

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
