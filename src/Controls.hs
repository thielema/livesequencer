-- |  controls are widgets that are:
-- * specified in the program text,
-- * displayed in the GUI,
-- * read while executing the program.

module Controls (
    module Controls,
    module ControlsBase,
    ) where

import ControlsBase
          ( Name, deconsName, Assignments,
            Value (Bool, Number), Values (boolValues, numberValues) )
import qualified ControlsBase as C
import qualified Program
import qualified Module
import qualified Rule
import qualified Term
import qualified Exception

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.IO.Class ( liftIO )

import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes ( text, checked, selection )
import Graphics.UI.WX.Events ( on, command, select )
import Graphics.UI.WX.Layout ( layout, container, row, column, widget )

import qualified Data.Map as M

import Data.Foldable ( forM_ )
import Control.Functor.HT ( void )


data Event = Event Name Value
    deriving Show



moduleName :: Module.Name
moduleName = Module.Name "Controls"

defltIdent :: Term.Term
defltIdent = read "deflt"

changeControllerModule ::
    Program.Program ->
    Event ->
    Exc.Exceptional Exception.Message Program.Program
changeControllerModule p0 (Event name val) =
    fmap (\p -> p{Program.controlValues =
                     updateValue name val $ Program.controlValues p}) .
    flip Program.replaceModule p0 .
    Module.addRule ( controllerRule name val ) =<<
    Exc.fromMaybe
        ( Module.inoutExceptionMsg moduleName
            "cannot find module for controller updates" )
        ( M.lookup moduleName $ Program.modules p0 )

updateValue ::
    Name -> Value -> Values -> Values
updateValue name val vals =
    case val of
        Bool b ->
            vals{boolValues = M.insert name b $ boolValues vals}
        Number x ->
            vals{numberValues = M.insert name x $ numberValues vals}


controllerRule ::
    Name -> Value -> Rule.Rule
controllerRule name val =
    case val of
        Bool b ->
            Rule.Rule
                ( read "checkBox" )
                [ Term.StringLiteral
                      ( Module.nameRange moduleName )
                      ( deconsName name ),
                  defltIdent ]
                ( Term.Node ( read $ show b ) [] )
        Number x ->
            Rule.Rule
                ( read "slider" )
                [ Term.StringLiteral
                      ( Module.nameRange moduleName )
                      ( deconsName name ),
                  read "lower",
                  read "upper",
                  defltIdent ]
                ( Term.Number ( Module.nameRange moduleName ) ( fromIntegral x ) )

create ::
    WX.Frame () ->
    Assignments ->
    (Event -> IO ()) ->
    IO ()
create frame controls sink = do
    size <- WX.get frame WX.outerSize
    void $ WXCMZ.windowDestroyChildren frame
    panel <- WX.panel frame []
    (cs,ss) <- MW.runWriterT $ MW.execWriterT $ forM_ (M.toList controls) $
            \ ( name, (_rng, con) ) ->
        case con of
            C.CheckBox val -> do
                cb <- liftIO $ WX.checkBox panel
                   [ text := deconsName name , checked := val ]
                liftIO $ set cb
                   [ on command := do
                         c <- get cb checked
                         sink $ Event name $ Bool c
                   ]
                MW.tell [ widget cb ]
            C.Slider lower upper val -> do
                sl <- liftIO $ WX.hslider panel False lower upper
                   [ selection := val ]
                sp <- liftIO $ WX.spinCtrl panel lower upper
                   [ selection := val ]
                liftIO $ set sl
                   [ on command := do
                         c <- get sl selection
                         set sp [ selection := c ]
                         sink $ Event name $ Number c
                   ]
                liftIO $ set sp
                   [ on select := do
                         c <- get sp selection
                         set sl [ selection := c ]
                         sink $ Event name $ Number c
                   ]
                MT.lift $ MW.tell [
                   WX.row 5 [ WX.hfill $ widget sl , widget sp,
                              WX.label (deconsName name) ]
                   ]
    set frame [
        layout :=
            container panel $ column 5 $
            WX.hfloatCenter (row 5 cs) : ss,
        WX.outerSize := size
        ]
