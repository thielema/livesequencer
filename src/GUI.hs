-- module GUI where

{- |
The program manages a number of threads:

GUI:
The GUI shall be responsive also if a program is loaded or a term is reduced.
Thus the GUI has its own thread.
If a GUI element requires a more complicated action,
it sends an Action message via the 'input' Chan to the 'machine'.
It does not have direct access to the 'program'.

machine:
This thread manages loading and parsing of modules
as well as the operation mode of the interpreter.
It gets most of its messages from the GUI
and sends its result as GuiUpdate via the 'output' Chan to the GUI.
It is the only thread that is allowed to modify the 'program'.
Thus it sequences all accesses to 'program'
and warrants atomic modification (a single read-write sequence)
even outside the STM monad.

execute:
This runs the interpreter.
It reduces expressions and sends according MIDI messages
or waits according to Wait events.
It can read the current state of the 'program'
but is not allowed to modify it.

ALSA:
With ALSA we can wait only for all kinds of events at once.
Thus this thread receives all incoming messages and distributes them
to the right receiver.
E.g. NoteOn events are sent to the GUI as text inserts
and Echo messages are sent to the 'execute' thread for handling Wait events.

HTTPServer:
Waits for and responds to incoming HTTP requests.
-}

import qualified IO
import qualified Term
import qualified Time
import qualified Program
import qualified Exception
import qualified Module
import qualified Controls
import qualified Rewrite
import qualified Option
import qualified Log
import Program ( Program )
import Term ( Term, Identifier, mainName )
import Option.Utility ( exitFailureMsg )
import Utility.Concurrent ( writeTMVar, writeTChanIO, liftSTM )
import Utility.WX ( cursor, editable, notebookSelection )

import qualified HTTPServer.GUI as HTTPGui

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes
           ( itemAppend, items, checkable, checked, clientSize,
             close, enabled, font, help, text, visible )
import Graphics.UI.WX.Controls
           ( Notebook, TextCtrl, wrap, focusOn, columns, listEvent,
             Align(AlignLeft, AlignRight), Wrap(WrapNone) )
import Graphics.UI.WX.Events
           ( on, closing, command )
import Graphics.UI.WX.Layout
           ( widget, container, layout, margin )
import Graphics.UI.WX.Types
           ( Color, rgb, fontFixed, Point2(Point), sz,
             varCreate, varSwap, varUpdate )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar, putMVar )
import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.STM.TChan ( TChan, newTChanIO, readTChan, writeTChan )
import Control.Concurrent.STM.TVar  ( TVar, newTVarIO, readTVarIO, readTVar, writeTVar )
import Control.Concurrent.STM.TMVar ( TMVar, newTMVarIO, putTMVar, readTMVar, takeTMVar )
import Control.Monad.STM ( STM )
import qualified Control.Monad.STM as STM

import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )

import qualified Graphics.UI.WXCore as WXCore
import qualified Graphics.UI.WXCore.WxcClassesAL as WXCAL
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WXCore.WxcDefs ( wxID_HIGHEST )

import qualified Graphics.UI.WXCore.Events as WXEvent
import qualified Event

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )
import Foreign.Marshal.Alloc ( alloca )
import qualified Foreign.C.Types as C

import qualified ALSA
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.MIDI.Message.Channel.Voice as VM

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( when, liftM, liftM2, forever )
import Control.Functor.HT ( void )
import Data.Foldable ( forM_ )
import Data.Traversable ( forM )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Exception ( bracket, finally, try )
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified System.FilePath as FilePath

import qualified Data.Accessor.Monad.Trans.State as AccM
import qualified Data.Accessor.Basic as Acc
import qualified Data.Accessor.Tuple as AccTuple

import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map as M

import qualified Data.Monoid as Mn

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Tuple.HT ( mapSnd )
import Data.Bool.HT ( if' )

import Prelude hiding ( log )


-- | read rules files, should contain definition for "main"
main :: IO ()
main = do
    IO.hSetBuffering IO.stderr IO.LineBuffering
    opt <- Option.get

    (mainMod, p) <-
        Exc.resolveT (exitFailureMsg . Exception.multilineFromMessage) $
            case Option.moduleNames opt of
                [] ->
                    return $
                        let name = Module.Name "Main"
                        in  (name, Program.singleton $ Module.empty name)
                names@(mainModName:_) ->
                    {-
                    If a file is not found, we setup an empty module.
                    If a file exists but contains parse errors
                    then we abort loading.
                    -}
                    fmap ((,) mainModName) $
                    flip MS.execStateT Program.empty $
                    mapM_
                        (\name -> do
                            epath <-
                                lift $ lift $ Exc.tryT $
                                Program.chaseFile (Option.importPaths opt)
                                    (Module.makeFileName name)
                            case epath of
                                Exc.Success path -> do
                                    voidStateT $
                                        Program.load (Option.importPaths opt)
                                            (Module.deconsName name) path
                                Exc.Exception _ ->
                                    voidStateT $ Exception.lift .
                                        Program.addModule (Module.empty name))
                        names

    input <- newChan
    output <- newTChanIO
    STM.atomically $ registerProgram output mainMod p
    ALSA.withSequencer opt $ \sq -> do
        flip finally (ALSA.stopQueue sq) $ WX.start $ do
            gui input output
            void $ forkIO $ machine input output (Option.importPaths opt) p sq
            void $ forkIO $
                HTTPGui.run
                    (HTTPGui.methods (writeTChanIO output . HTTP))
                    (Option.httpOption opt)


-- | messages that are sent from GUI to machine
data Action =
     Execution Execution
   | Modification Modification
   | Control Controls.Event

data Execution =
    Mode Event.WaitMode | Restart | Stop | NextStep |
    PlayTerm MarkedText | ApplyTerm MarkedText

data Modification =
     Load FilePath
   | NewModule
   | CloseModule Module.Name
   | FlushModules Module.Name
   | RefreshModule (Maybe (MVar HTTPGui.Feedback)) Module.Name String Int
         -- ^ MVar of the HTTP server, modulename, sourcetext, position


-- | messages that are sent from machine to GUI
data GuiUpdate =
     ReductionSteps { _steps :: [ Rewrite.Message ] }
   | CurrentTerm { _currentTerm :: String }
   | Exception { _message :: Exception.Message }
   | Register { _mainModName :: Module.Name, _modules :: M.Map Module.Name Module.Module }
   | Refresh { _moduleName :: Module.Name, _content :: String, _position :: Int }
   | InsertPage { _activate :: Bool, _module :: Module.Module }
   | DeletePage Module.Name
   | RenamePage Module.Name Module.Name
   | RebuildControls Controls.Assignments
   | InsertText { _insertedText :: String }
   | StatusLine { _statusLine :: String }
   | HTTP HTTPGui.GuiUpdate
   | Running { _runningMode :: Event.WaitMode }
   | ResetDisplay


exceptionToGUI ::
    TChan GuiUpdate ->
    Exc.ExceptionalT Exception.Message STM () ->
    STM ()
exceptionToGUI output =
    Exc.resolveT (writeTChan output . Exception)

exceptionToGUIIO ::
    TChan GuiUpdate ->
    Exc.ExceptionalT Exception.Message IO () ->
    IO ()
exceptionToGUIIO output =
    Exc.resolveT (writeTChanIO output . Exception)

parseTerm ::
    (Monad m, IO.Input a) =>
    MarkedText -> Exc.ExceptionalT Exception.Message m a
parseTerm (MarkedText pos str) =
    case Parsec.parse
             (Parsec.setPosition pos
              >>
              Parsec.between
                 (Token.whiteSpace Term.lexer)
                 Parsec.eof
                 IO.input)
             "" str of
        Left msg ->
            Exc.throwT $ Exception.messageFromParserError msg
        Right t -> return t

inoutExceptionMsg :: Module.Name -> String -> Exception.Message
inoutExceptionMsg moduleName msg =
    Exception.Message Exception.InOut
        (Program.dummyRange (Module.deconsName moduleName)) msg


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

formatModuleList :: [Module.Name] -> String
formatModuleList =
    List.intercalate ", " . map Module.deconsName


{-
We do not put the program update into a big a STM
because loading new imported modules may take a while
and blocking access to 'program'
would block the read access by the interpreter.
-}
modifyModule ::
    [ FilePath ] ->
    TVar Program ->
    TChan GuiUpdate ->
    Module.Name ->
    String ->
    Int ->
    IO (Maybe Exception.Message)
modifyModule importPaths program output moduleName sourceCode pos = do
    p <- readTVarIO program
    Exception.switchT
        (\e -> do
            writeTChanIO output $ Exception e
            return $ Just e)
        (\(newP, updates) -> do
            STM.atomically $ do
                mapM_ ( writeTChan output ) updates
                writeTVar program newP
--            Log.put "parsed and modified OK"
            return Nothing) $ do
        let exception =
                Exception.Message Exception.Parse
                    (Program.dummyRange $ Module.deconsName moduleName)
        previous <-
            case M.lookup moduleName $ Program.modules p of
                Nothing ->
                    Exc.throwT $ exception $
                    Module.tellName moduleName ++ " does no longer exist"
                Just m -> return m
        m <-
            Exception.lift $ Module.parse
                (Module.deconsName moduleName)
                (Module.sourceLocation previous) sourceCode
        {-
        My first thought was that renaming of modules
        should be generally forbidden via HTTP.
        My second thought was that renaming of modules
        can be easily allowed or forbidden using the separation marker.
        Actually currently renaming via HTTP is not possible,
        because the separation marker is not allowed before the 'module' line.
        If you like to strictly forbid renaming in some circumstances,
        then make 'allowRename' a parameter of the function.
        -}
        let allowRename = True
        MW.runWriterT $ do
            p1 <-
                if' (moduleName == Module.name m)
                    (lift $ Exception.lift $ Program.replaceModule m p) $
                if' allowRename (do
                     lift $ Exc.assertT
                         (exception $ Module.tellName (Module.name m) ++ " already exists")
                         (not $ M.member (Module.name m) $ Program.modules p)
                     MW.tell
                         [ RenamePage moduleName (Module.name m) ]
                     lift $ Exception.lift $ Program.addModule m $
                         Program.removeModule moduleName p) $
                (lift $ Exc.throwT $ exception
                    "module name does not match page name and renaming is disallowed")
            p2 <- lift $ Program.chaseImports importPaths m p1
            MW.tell $ map (InsertPage False) $ M.elems $
                M.difference ( Program.modules p2 ) ( Program.modules p1 )
            -- Refresh must happen after a Rename
            MW.tell [ Refresh (Module.name m) sourceCode pos,
                      RebuildControls $ Program.controls p2 ]
            return p2

registerProgram :: TChan GuiUpdate -> Module.Name -> Program -> STM ()
registerProgram output mainModName p = do
    writeTChan output $ Register mainModName $ Program.modules p
    writeTChan output $ RebuildControls $ Program.controls p

updateProgram :: TVar Program -> TChan GuiUpdate -> Program -> STM ()
updateProgram program output p = do
    liftSTM $ writeTVar program p
    liftSTM $ writeTChan output $ RebuildControls $ Program.controls p


{-
This runs concurrently
and is fed with changes to the modules by the GUI.
It parses them and provides the parsed modules to the execution engine.
Since parsing is a bit of work
we can keep the GUI and the execution of code going while parsing.
-}
machine :: Chan Action -- ^ machine reads program text from here
                   -- (module name, module contents)
        -> TChan GuiUpdate -- ^ and writes output to here
                   -- (log message (for highlighting), current term)
        -> [FilePath]
        -> Program -- ^ initial program
        -> ALSA.Sequencer SndSeq.DuplexMode
        -> IO ()
machine input output importPaths progInit sq = do
    program <- newTVarIO progInit
    term <- newTMVarIO mainName
    waitChan <- newChan

    void $ forkIO $ forever $ do
        action <- readChan input
        let withMode mode transaction = do
                writeChan waitChan $ Event.ModeChange mode
                STM.atomically $ do
                    writeTChan output $ Running mode
                    transaction
        case action of
            Control event -> do
                Log.put $ show event
                STM.atomically $ exceptionToGUI output $ do
                    p <- lift $ readTVar program
                    p' <- Exception.lift $ Controls.changeControllerModule p event
                    lift $ writeTVar program p'
                    -- return $ Controls.getControllerModule p'
                -- Log.put $ show m

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
                    NextStep -> writeChan waitChan Event.NextStep
                    PlayTerm txt -> exceptionToGUIIO output $ do
                        t <- parseTerm txt
                        lift $ ALSA.quietContinueQueue sq
                        lift $ withMode Event.RealTime $ writeTMVar term t
                    ApplyTerm txt -> exceptionToGUIIO output $ do
                        fterm <- parseTerm txt
                        case fterm of
                            Term.Node f xs ->
                                lift $ STM.atomically $ do
                                    t0 <- readTMVar term
                                    let t1 = Term.Node f (xs++[t0])
                                    writeTMVar term t1
                                    writeTChan output $ CurrentTerm $ show t1
                                    writeTChan output $ StatusLine $
                                        "applied function term " ++
                                        show (markedString txt)
                            _ ->
                                Exc.throwT .
                                Exception.Message Exception.Parse (Term.termRange fterm) $
                                "tried to apply the non-function term " ++
                                show (markedString txt)

            Modification modi ->
                case modi of
                    RefreshModule feedback moduleName sourceCode pos -> do
                        Log.put $
                            Module.tellName moduleName ++
                            " has new input\n" ++ sourceCode
                        case feedback of
                            Nothing ->
                                void $
                                modifyModule importPaths program output moduleName sourceCode pos
                            Just mvar -> do
                                x <- modifyModule importPaths program output moduleName sourceCode pos
                                putMVar mvar $ Exc.Success
                                    (fmap Exception.multilineFromMessage x,
                                     sourceCode)

                    Load filePath -> do
                        Log.put $
                            "load " ++ filePath ++ " and all its dependencies"
                        exceptionToGUIIO output $ do
                            let stem = FilePath.takeBaseName filePath
                            p <-
                                Program.load importPaths stem filePath
                                    Program.empty
                            lift $ do
                                ALSA.stopQueue sq
                                withMode Event.RealTime $ do
                                    writeTVar program p
                                    writeTMVar term mainName
                                    registerProgram output (Module.Name stem) p
                                ALSA.continueQueue sq
                                Log.put "chased and parsed OK"

                    NewModule ->
                        STM.atomically $ do
                            prg <- readTVar program
                            let modName =
                                   head $
                                   filter (not . flip M.member (Program.modules prg)) $
                                   map (Module.Name . ("New"++)) $
                                   "" : map show (iterate (1+) (1::Integer))

                                modu = Module.empty modName
                            case Program.addModule modu prg of
                                Exc.Exception e ->
                                    error ("new module has no declarations and thus should not lead to conflicts with existing modules - " ++ Exception.statusFromMessage e)
                                Exc.Success newPrg ->
                                    liftSTM $ updateProgram program output newPrg
                            liftSTM $ writeTChan output $ InsertPage True modu

                    CloseModule modName ->
                        STM.atomically $ exceptionToGUI output $
                            Exc.mapExceptionT
                                (inoutExceptionMsg modName .
                                 ("cannot close module: " ++)) $ do
                            prg <- liftSTM $ readTVar program
                            let modules = Program.modules prg
                                importingModules =
                                    M.keys $
                                    M.filter (elem modName . map Module.source .
                                              Module.imports) $
                                    M.delete modName modules
                            flip Exc.assertT (null importingModules) $
                                "it is still imported by " ++
                                formatModuleList importingModules
                            flip Exc.assertT (M.member modName modules) $
                                "it does not exist"
                            flip Exc.assertT (M.size modules > 1) $
                                "there must remain at least one module"
                            liftSTM $ updateProgram program output $
                                Program.removeModule modName prg
                            liftSTM $ writeTChan output $ DeletePage modName

                    FlushModules modName ->
                        STM.atomically $ do
                            prg <- readTVar program
                            let (removed, minPrg) = Program.minimize modName prg
                            updateProgram program output minPrg
                            Fold.mapM_ (writeTChan output . DeletePage) removed

    void $ forkIO $
        Event.listen sq
            ( writeTChanIO output . InsertText . formatPitch )
            waitChan
    ALSA.startQueue sq
    Event.runState $
        execute program term ( writeTChan output ) sq waitChan


execute :: TVar Program
                  -- ^ current program (GUI might change the contents)
        -> TMVar Term -- ^ current term
        -> ( GuiUpdate -> STM () ) -- ^ sink for messages (show current term)
        -> ALSA.Sequencer SndSeq.DuplexMode -- ^ for playing MIDI events
        -> Chan Event.WaitResult
        -> MS.StateT Event.State IO ()
execute program term output sq waitChan =
    forever $ do
        (mdur,outputs) <- MW.runWriterT $ do
            waiting <- lift $ AccM.get Event.stateWaiting
            when waiting $ writeUpdate ResetDisplay
            executeStep program term
                (STM.atomically . output . Exception) sq
        liftIO $ STM.atomically $ mapM_ output outputs
        Event.wait sq waitChan mdur

executeStep ::
    TVar Program ->
    TMVar Term ->
    ( Exception.Message -> IO () ) ->
    ALSA.Sequencer SndSeq.DuplexMode ->
    MW.WriterT [ GuiUpdate ]
        ( MS.StateT Event.State IO ) ( Maybe Event.Time )
executeStep program term writeExcMsg sq =
    Exception.switchT
        (\e -> do
            liftIO $ ALSA.stopQueue sq
            -- writeChan waitChan $ Event.ModeChange Event.SingleStep
            writeUpdate $ Exception e
            writeUpdate $ Running Event.SingleStep
            {-
            We have to alter the mode directly,
            since the channel is only read when we wait for a duration other than Nothing
            -}
            lift $ AccM.set Event.stateWaitMode Event.SingleStep
            return Nothing)
        (\(x,s) -> do
            {-
            exceptions on processing an event are not fatal and we keep running
            -}
            wait <- Exc.resolveT
                (fmap (const Nothing) . writeUpdate . Exception)
                (Exc.mapExceptionalT lift $
                 Event.play sq writeExcMsg x)

            waitMode <- lift $ AccM.get Event.stateWaitMode
            waiting  <- lift $ AccM.get Event.stateWaiting
            {-
            This way the term will be pretty printed in the GUI thread
            which may block the GUI thread.
            However evaluating it here may defer playing notes,
            which is not better.
            -}
            when (waiting || waitMode /= Event.RealTime) $
                writeUpdate $ CurrentTerm $ show s
            return wait)
        (Exc.mapExceptionalT (MW.mapWriterT (liftIO . STM.atomically)) $
            flip Exc.catchT (\(pos,msg) -> do
                liftSTM $ putTMVar term mainName
                Exc.throwT $ Exception.Message Exception.Term pos msg) $ do
            t <- liftSTM $ takeTMVar term
            p <- liftSTM $ readTVar program
                {- this happens anew at each click
                   since the program text might have changed in the editor -}
            (s,log) <-
                Exc.mapExceptionalT
                    (fmap (\(ms,log) -> liftM2 (,) ms (return log)) .
                     MW.runWriterT) $
                Rewrite.runEval p (Rewrite.forceHead t)
            lift $ writeUpdate $ ReductionSteps log
            case Term.viewNode s of
                Just (":", [x, xs]) -> do
                    liftSTM $ putTMVar term xs
                    return (x,s)
                Just ("[]", []) -> do
                    lift $ writeUpdate $ CurrentTerm $ show s
                    Exc.throwT (Term.termRange s, "finished.")
                _ -> do
                    lift $ writeUpdate $ CurrentTerm $ show s
                    Exc.throwT (Term.termRange s,
                        "I do not know how to handle this term: " ++ show s))


voidStateT :: (Monad m) => (s -> m s) -> MS.StateT s m ()
voidStateT f = MS.StateT $ liftM ((,) ()) . f


writeUpdate ::
    (Monad m) =>
    GuiUpdate -> MW.WriterT [GuiUpdate] m ()
writeUpdate update = MW.tell [update]


-- | following code taken from http://snipplr.com/view/17538/
myEventId :: Int
myEventId = wxID_HIGHEST+100
    -- the custom event ID, avoid clash with Graphics.UI.WXCore.Types.varTopId

-- | the custom event is registered as a menu event
createMyEvent :: IO (WXCore.CommandEvent ())
createMyEvent =
    WXCAL.commandEventCreate WXCMZ.wxEVT_COMMAND_MENU_SELECTED myEventId

registerMyEvent :: WXCore.EvtHandler a -> IO () -> IO ()
registerMyEvent win io = WXEvent.evtHandlerOnMenuCommand win myEventId io


{-
The order of widget creation is important
for cycling through widgets using tabulator key.
-}
gui :: Chan Action -- ^  the gui writes here
      -- (if the program text changes due to an edit action)
    -> TChan GuiUpdate -- ^ the machine writes here
      -- (a textual representation of "current expression")
    -> IO ()
gui input output = do
    panels <- newIORef M.empty

    frameError <- newFrameError

    frameControls <- WX.frame [ text := "controls" ]

    f <- WX.frame
        [ text := "live-sequencer", visible := False
        ]

    out <- newChan

    void $ forkIO $ forever $ do
        writeChan out =<< STM.atomically (readTChan output)
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

    newModuleItem <- WX.menuItem fileMenu
        [ text := "&New module\tCtrl-Shift-N",
          help := "add a new empty module" ]

    closeModuleItem <- WX.menuItem fileMenu
        [ text := "&Close module\tCtrl-W",
          help := "close the active module" ]

    flushModulesItem <- WX.menuItem fileMenu
        [ text := "&Flush modules",
          help := "close all modules that are not transitively imported by the active module" ]

    WX.menuLine fileMenu

    quitItem <- WX.menuQuit fileMenu []


    execMenu <- WX.menuPane [text := "&Execution"]

    refreshItem <- WX.menuItem execMenu
        [ text := "&Refresh\tCtrl-R",
          help :=
              "parse the edited module and if successful " ++
              "rename the page to the modified module name, " ++
              "load new imported modules and " ++
              "replace the executed program" ]
    WX.menuLine execMenu
    realTimeItem <- WX.menuItem execMenu
        [ text := "Real time\tCtrl-1",
          checkable := True,
          checked := True,
          help := "pause according to Wait elements" ]
    slowMotionItem <- WX.menuItem execMenu
        [ text := "Slow motion\tCtrl-2",
          checkable := True,
          help := "pause between every list element" ]
    singleStepItem <- WX.menuItem execMenu
        [ text := "Single step\tCtrl-3",
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
    applyTermItem <- WX.menuItem execMenu
        [ text := "Apply term\tCtrl-Y",
          help :=
              "apply marked expression as function to the current term, " ++
              "the execution mode remains the same, " ++
              "example terms: (merge track) or (flip append track)" ]
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
          on command := writeChan input (Execution NextStep),
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
                      else WXEvent.propagateEvent ]

    windowMenuItem "errors" $ errorFrame frameError
    windowMenuItem "controls" frameControls
    WX.menuLine windowMenu
    reducerVisibleItem <- WX.menuItem windowMenu
        [ text := "current term",
          checkable := True,
          checked := True,
          help := "show or hide current term - " ++
                  "hiding may improve performance drastically" ]


    splitter <- WX.splitterWindow p []

    nb <- WX.notebook splitter [ ]

    reducer <-
        WX.textCtrl splitter
            [ font := fontFixed, editable := False, wrap := WrapNone ]

    status <- WX.statusField
        [ text := "Welcome to interactive music composition with Haskell" ]


    let handleException moduleName act = do
            result <- try act
            case result of
                Left err ->
                    writeTChanIO output $ Exception $
                    inoutExceptionMsg moduleName $
                    Err.ioeGetErrorString err
                Right () -> return ()

    set loadItem [
          on command := do
              mfilename <- WX.fileOpenDialog
                  f False {- change current directory -} True
                  "Load Haskell program" haskellFilenames "" ""
              forM_ mfilename $ writeChan input . Modification . Load
          ]

    set reloadItem [
          on command := do
              (moduleName, pnl) <-
                  getFromNotebook nb =<< readIORef panels
              let path = sourceLocation pnl

              handleException moduleName $ do
                  content <- readFile path
                  set (editor pnl) [ text := content ]
                  set status [
                      text := Module.tellName moduleName ++ " reloaded from " ++ path ]
          ]

    let getCurrentModule = do
            (moduleName, pnl) <-
                getFromNotebook nb =<< readIORef panels
            content <- get (editor pnl) text
            return (sourceLocation pnl, moduleName, content)
        saveModule (path, moduleName, content) =
            handleException moduleName $ do
                -- Log.put path
                writeFile path content
                set status [
                    text := Module.tellName moduleName ++ " saved to " ++ path ]

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
                  ("Save " ++ Module.tellName moduleName) haskellFilenames path file
              forM_ mfilename $ \fileName -> do
                  saveModule (fileName, moduleName, content)
                  modifyIORef panels $
                      M.adjust
                          (\pnl -> pnl { sourceLocation = fileName })
                          moduleName
          ]


    set newModuleItem [
          on command :=
              writeChan input $ Modification NewModule
          ]

    set closeModuleItem [
          on command :=
              writeChan input . Modification . CloseModule . fst
                  =<< getFromNotebook nb =<< readIORef panels
          ]

    set flushModulesItem [
          on command :=
              writeChan input . Modification . FlushModules . fst
                  =<< getFromNotebook nb =<< readIORef panels
          ]

    let refreshProgram (moduleName, pnl) = do
            s <- get (editor pnl) text
            pos <- get (editor pnl) cursor
            writeChan input $ Modification $ RefreshModule Nothing moduleName s pos

            updateErrorLog frameError $ Seq.filter $
                \(Exception.Message _ errorRng _) ->
                    Module.deconsName moduleName /=
                    Pos.sourceName (Term.start errorRng)

    set refreshItem
        [ on command := do
            refreshProgram =<< getFromNotebook nb =<< readIORef panels
            -- mapM_ refreshProgram pnls
            ]

    set playTermItem
        [ on command :=
            writeChan input . Execution . PlayTerm
                =<< uncurry getMarkedExpr . mapSnd editor
                =<< getFromNotebook nb
                =<< readIORef panels ]

    set applyTermItem
        [ on command :=
            writeChan input . Execution . ApplyTerm
                =<< uncurry getMarkedExpr . mapSnd editor
                =<< getFromNotebook nb
                =<< readIORef panels ]

    waitDuration <- newIORef $ Time.milliseconds 500

    let updateSlowMotionDur = do
            dur <- readIORef waitDuration
            writeChan input $ Execution $ Mode $ Event.SlowMotion dur
        slowmoUnit = Time.milliseconds 100

    set fasterItem [
        on command := do
            modifyIORef waitDuration $
                \d -> max slowmoUnit (Time.sub d slowmoUnit)
            updateSlowMotionDur
            d <- readIORef waitDuration
            set status [ text :=
                "decreased pause to " ++ Time.format d ] ]

    set slowerItem [
        on command := do
            modifyIORef waitDuration $ Mn.mappend slowmoUnit
            updateSlowMotionDur
            d <- readIORef waitDuration
            set status [ text :=
                "increased pause to " ++ Time.format d ] ]

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

    let initSplitterPosition = 0 {- equal division of heights -}
    newIORef initSplitterPosition >>= \splitterPosition ->
        set reducerVisibleItem
            [ on command := do
                 b <- get reducerVisibleItem checked
                 isSplit <- WXCMZ.splitterWindowIsSplit splitter
                 when (b /= isSplit) $ void $
                     if b
                       then WXCMZ.splitterWindowSplitHorizontally
                                    splitter nb reducer =<<
                                readIORef splitterPosition
                       else do
                            writeIORef splitterPosition =<<
                                WXCMZ.splitterWindowGetSashPosition splitter
                            WXCMZ.splitterWindowUnsplit splitter reducer
            ]

    {-
    Without this dummy page the notebook sometimes gets a very small height,
    although we explicitly set the splitter position to 0 (= balanced tiling).
    However the imbalance is not reproducable.
    Maybe this is a race condition.
    -}
    do
       pnl <- displayModule nb (Module.empty $ Module.Name "Dummy")
       void $ WXCMZ.notebookAddPage nb (panel pnl) "Dummy" True (-1)

    set f [
            layout :=
                container p $ margin 5 $
                WX.fill $
                    WX.hsplit splitter
                        5 {- sash width -} initSplitterPosition
                        (widget nb) (widget reducer)
            , WX.statusBar := [status]
            , WX.menuBar   := [fileMenu, execMenu, windowMenu]
            , visible := True
            , clientSize := sz 1280 720
          ]

    onErrorSelection frameError $ \(Exception.Message typ errorRng _descr) -> do
        let moduleIdent =
                Module.Name $
                Pos.sourceName $ Term.start errorRng
        pnls <- liftIO $ readIORef panels
        pnl <- MaybeT.MaybeT $ return $ M.lookupIndex moduleIdent pnls
        liftIO $ set nb [ notebookSelection := pnl ]
        let activateText textField = do
                h <- MaybeT.MaybeT $ return $
                     M.lookup moduleIdent textField
                (i,j) <- liftIO $ textRangeFromRange h errorRng
                liftIO $ set h [ cursor := i ]
                liftIO $ WXCMZ.textCtrlSetSelection h i j
        case typ of
            Exception.Parse ->
                activateText $ fmap editor pnls
            Exception.Term ->
                activateText $ fmap highlighter pnls
            Exception.InOut ->
                return ()

    let closeOther =
            writeIORef appRunning False >>
            close (errorFrame frameError) >> close frameControls
    set quitItem [ on command := closeOther >> close f]
    set f [ on closing := closeOther >> WXEvent.propagateEvent
        {- 'close f' would trigger the closing handler again -} ]
    focusOn f


    highlights <- varCreate M.empty

    registerMyEvent f $ do
        msg <- readChan out
        case msg of
            CurrentTerm sr -> do
                get reducerVisibleItem checked >>=
                    flip when ( set reducer [ text := sr, cursor := 0 ] )

            ReductionSteps steps -> do
                hls <- fmap (fmap highlighter) $ readIORef panels
                visibleModule <- fmap fst $ getFromNotebook nb hls
                let highlight ::
                        Int -> Int -> Int -> [Identifier] -> IO ()
                    highlight r g b idents = do
                        let m = M.fromListWith (++) $
                                filter ((visibleModule==) . fst) $
                                map (\ident -> (Module.nameFromIdentifier ident, [ident])) idents
                        void $ varUpdate highlights $ M.unionWith (++) $ m
                        setColor hls ( rgb r g b ) m

                let prep step =
                        case step of
                            Rewrite.Step target -> (AccTuple.first3, (target:))
                            Rewrite.Rule rule   -> (AccTuple.second3, (rule:))
                            Rewrite.Data origin -> (AccTuple.third3, (origin:))
                    (targets, rules, origins) =
                        foldr (uncurry Acc.modify) ([],[],[]) $
                        map prep steps

                highlight 0 200 200 targets
                highlight 200 0 200 rules
                highlight 200 200 0 origins

            ResetDisplay -> do
                hls <- fmap (fmap highlighter) $ readIORef panels
                setColor hls WXCore.white
                    =<< varSwap highlights M.empty

            Exception exc -> do
                addToErrorLog frameError exc
                set status [ text := Exception.statusFromMessage exc ]

            -- update highlighter text field only if parsing was successful
            Refresh moduleName s pos -> do
                pnls <- readIORef panels
                Fold.mapM_
                    (\pnl -> set (highlighter pnl) [ text := s, cursor := pos ])
                    (M.lookup moduleName pnls)
                set status [ text :=
                    Module.tellName moduleName ++ " reloaded into interpreter" ]

            InsertText str -> do
                pnl <- fmap snd $ getFromNotebook nb =<< readIORef panels
                WXCMZ.textCtrlWriteText (editor pnl) str
                set status [ text :=
                    "inserted note from external controller" ]

            StatusLine str -> do
                set status [ text := str ]

            Register mainModName mods -> do
                void $ WXCMZ.notebookDeleteAllPages nb
                (writeIORef panels =<<) $ forM mods $ \modu -> do
                    pnl <- displayModule nb modu
                    void $ WXCMZ.notebookAddPage nb (panel pnl)
                        (Module.deconsName $ Module.name modu)
                        (Module.name modu == mainModName) (-1)
                    return pnl

                updateErrorLog frameError (const Seq.empty)

                set status [ text :=
                    "modules loaded: " ++ formatModuleList ( M.keys mods ) ]

            InsertPage act modu -> do
                pnls <- readIORef panels
                pnl <- displayModule nb modu
                let modName = Module.name modu
                    newPnls = M.insert modName pnl pnls
                writeIORef panels newPnls
                success <-
                    WXCMZ.notebookInsertPage nb
                        (M.findIndex modName newPnls) (panel pnl)
                        (Module.deconsName modName) act (-1)
                {- FIXME:
                if the page cannot be added, we get an inconsistency -
                how to solve that?
                -}
                if success
                  then
                    set status [ text := "new " ++ Module.tellName modName ]
                  else
                    writeTChanIO output $ Exception $
                    inoutExceptionMsg modName $
                    "Panic: cannot add page for the module"

            DeletePage modName -> do
                pnls <- readIORef panels
                forM_ ( M.lookupIndex modName pnls ) $
                    WXCMZ.notebookDeletePage nb
                writeIORef panels $ M.delete modName pnls
                set status [ text := "closed " ++ Module.tellName modName ]

            RenamePage fromName toName -> do
                pnls <- readIORef panels
                forM_
                    ( liftM2 (,)
                        ( M.lookupIndex fromName pnls )
                        ( M.lookup fromName pnls ) ) $ \(i,pnl) -> do
                    success <- WXCMZ.notebookRemovePage nb i
                    when (not success) $
                        writeTChanIO output $ Exception $
                        inoutExceptionMsg fromName $
                        "Panic: cannot remove page for renaming module"
                    let newPnls =
                            M.insert toName pnl $ M.delete fromName pnls
                    writeIORef panels newPnls
                    forM_ ( M.lookupIndex toName newPnls ) $ \j ->
                        WXCMZ.notebookInsertPage nb j (panel pnl)
                            (Module.deconsName toName) True (-1)
                set status [ text := "renamed " ++ Module.tellName fromName ++
                                     " to " ++ Module.tellName toName ]

            RebuildControls ctrls ->
                Controls.create frameControls ctrls $
                    writeChan input . Control

            Running mode -> do
                case mode of
                    Event.RealTime -> do
                        set status [ text := "interpreter in real-time mode" ]
                        activateRealTime
                    Event.SlowMotion dur -> do
                        set status [ text :=
                            ("interpreter in slow-motion mode with pause " ++
                             Time.format dur) ]
                        activateSlowMotion
                    Event.SingleStep -> do
                        set status [ text :=
                            "interpreter in single step mode," ++
                            " waiting for next step" ]
                        activateSingleStep

            HTTP request -> do
                pnls <- readIORef panels
                HTTPGui.update
                    (\contentMVar name newContent pos ->
                        writeChan input $ Modification $
                        RefreshModule (Just contentMVar) name newContent pos)
                    status (fmap editor pnls) request


data FrameError =
    FrameError {
        errorFrame :: WX.Frame (),
        errorLog :: WX.ListCtrl (),
        errorText :: WX.TextCtrl (),
        errorList :: IORef (Seq.Seq Exception.Message)
    }

newFrameError :: IO FrameError
newFrameError = do
    frame <- WX.frame [ text := "errors" ]

    pnl <- WX.panel frame [ ]

    splitter <- WX.splitterWindow pnl [ ]

    log <- WX.listCtrl splitter
        [ columns :=
              ("Module", AlignLeft, 120) :
              ("Row", AlignRight, -1) :
              ("Column", AlignRight, -1) :
              ("Type", AlignLeft, -1) :
              ("Description", AlignLeft, 500) :
              []
        ]
    list <- newIORef Seq.empty

    txt <- WX.textCtrl splitter
        [ font := fontFixed, wrap := WrapNone, editable := False ]

    let rec =
            FrameError {
                errorFrame = frame,
                errorLog = log,
                errorText = txt,
                errorList = list
            }

    clearLog <- WX.button pnl
        [ text := "Clear",
          on command := do
              updateErrorLog rec (const Seq.empty)
              set txt [ text := "" ] ]

    set frame
        [ layout := container pnl $ margin 5 $ WX.column 5 $
             [ WX.fill $ WX.hsplit splitter 5 0 (widget log) (widget txt),
               WX.hfloatLeft $ widget clearLog ]
        , clientSize := sz 500 300
        ]

    return rec

onErrorSelection ::
    FrameError -> (Exception.Message -> MaybeT.MaybeT IO ()) -> IO ()
onErrorSelection r act =
    set (errorLog r)
        [ on listEvent := \ev -> void $ MaybeT.runMaybeT $ do
              WXEvent.ListItemSelected n <- return ev
              errors <- liftIO $ readIORef (errorList r)
              let msg@(Exception.Message _typ _errorRng descr) =
                      Seq.index errors n
              liftIO $ set (errorText r) [ text := descr ]
              act msg
        ]

updateErrorLog ::
    FrameError ->
    (Seq.Seq Exception.Message -> Seq.Seq Exception.Message) ->
    IO ()
updateErrorLog r f = do
    errors <- readIORef (errorList r)
    let newErrors = f errors
    writeIORef (errorList r) newErrors
    set (errorLog r) [ items :=
          map Exception.lineFromMessage $ Fold.toList newErrors ]

addToErrorLog ::
    FrameError -> Exception.Message -> IO ()
addToErrorLog r exc = do
    itemAppend (errorLog r) $ Exception.lineFromMessage exc
    modifyIORef (errorList r) (Seq.|> exc)


data Panel =
    Panel {
        panel :: WX.SplitterWindow (),
        editor, highlighter :: WX.TextCtrl (),
        sourceLocation :: FilePath
    }

displayModule ::
    WXCore.Window b ->
    Module.Module ->
    IO Panel
displayModule nb modu = do
    psub <- WX.splitterWindow nb []
    ed <- WX.textCtrl psub [ font := fontFixed, wrap := WrapNone ]
    hl <- WX.textCtrlRich psub
        [ font := fontFixed, wrap := WrapNone, editable := False ]
    set ed [ text := Module.sourceText modu ]
    set hl [ text := Module.sourceText modu ]
    void $ WXCMZ.splitterWindowSplitVertically psub ed hl 0
{-
    set psub [
        layout :=
            WX.vsplit psub 5 0 (WX.fill $ widget ed) (WX.fill $ widget hl) ]
-}
    return $ Panel psub ed hl $ Module.sourceLocation modu


getFromNotebook ::
    Notebook b -> M.Map Module.Name a -> IO (Module.Name, a)
getFromNotebook nb m =
    fmap (flip M.elemAt m) $ get nb notebookSelection

textPosFromSourcePos ::
    TextCtrl a -> Pos.SourcePos -> IO Int
textPosFromSourcePos textArea pos =
    WXCMZ.textCtrlXYToPosition textArea
       $ Point (Pos.sourceColumn pos - 1)
               (Pos.sourceLine   pos - 1)

sourcePosFromTextColumnRow ::
    Module.Name -> (Int, Int) -> Pos.SourcePos
sourcePosFromTextColumnRow (Module.Name name) (col, line) =
    Pos.newPos name (line+1) (col+1)

textRangeFromRange ::
    TextCtrl a -> Term.Range -> IO (Int, Int)
textRangeFromRange textArea rng = do
    from <- textPosFromSourcePos textArea $ Term.start rng
    to   <- textPosFromSourcePos textArea $ Term.end   rng
    return (from, to)

textRangeFromSelection ::
    TextCtrl a -> IO (Int, Int)
textRangeFromSelection textArea =
    alloca $ \fromPtr ->
    alloca $ \toPtr -> do
        void $ WXCMZ.textCtrlGetSelection textArea fromPtr toPtr
        liftM2 (,)
            (fmap fromIntegral $ peek (fromPtr :: Ptr C.CInt))
            (fmap fromIntegral $ peek (toPtr :: Ptr C.CInt))

textColumnRowFromPos ::
    TextCtrl a -> Int -> IO (Int, Int)
textColumnRowFromPos textArea pos =
    alloca $ \rowPtr ->
    alloca $ \columnPtr -> do
        void $ WXCMZ.textCtrlPositionToXY textArea pos columnPtr rowPtr
        liftM2 (,)
            (fmap fromIntegral $ peek columnPtr)
            (fmap fromIntegral $ peek rowPtr)

setColor ::
    (Ord k) =>
    M.Map k (TextCtrl a) ->
    Color ->
    M.Map k [Identifier] ->
    IO ()
setColor highlighters hicolor positions = do
    forM_ (M.intersectionWith (,) highlighters positions) $
        \(hl, idents) -> do
            attr <- WXCMZ.textCtrlGetDefaultStyle hl
            bracket
                (WXCMZ.textAttrGetBackgroundColour attr)
                (WXCMZ.textAttrSetBackgroundColour attr) $ const $ do
                    WXCMZ.textAttrSetBackgroundColour attr hicolor
                    forM_ idents $ \ ident -> do
                        (from, to) <-
                            textRangeFromRange hl $ Term.range ident
                        WXCMZ.textCtrlSetStyle hl from to attr


data MarkedText =
    MarkedText {
        _markedPosition :: Pos.SourcePos,
        markedString :: String
    }

getMarkedExpr :: Module.Name -> TextCtrl () -> IO MarkedText
getMarkedExpr modu ed = do
    marked <- WXCMZ.textCtrlGetStringSelection ed
    if null marked
      then do
          (i,line) <-
              textColumnRowFromPos ed =<< get ed cursor
          content <- WXCMZ.textCtrlGetLineText ed line
{- simpler but inefficient
          content <- get ed text
          i <- get ed cursor
-}
          case splitAt i content of
              (prefix,suffix) ->
                  let identLetter c = Char.isAlphaNum c || c == '_' || c == '.'
                  in  return $
                      MarkedText
                          (sourcePosFromTextColumnRow modu (i - length prefix, line))
                          ((reverse $ takeWhile identLetter $ reverse prefix)
                           ++
                           takeWhile identLetter suffix)
      else do
          (from, _to) <- textRangeFromSelection ed
          pos <- textColumnRowFromPos ed from
          return $ MarkedText (sourcePosFromTextColumnRow modu pos) marked
