-- |  controls are widgets that are:
-- * specified in the program text,
-- * displayed in the GUI,
-- * read while executing the program.

module Controls (
    module Controls,
    module ControlsBase,
    ) where

import ControlsBase
import qualified Program
import qualified Module
import qualified Rule
import qualified Term
import qualified Exception

import qualified Control.Monad.Exception.Synchronous as Exc

import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes ( text, checked )
import Graphics.UI.WX.Events ( on, command )
import Graphics.UI.WX.Layout ( layout, container, row, widget )

import qualified Data.Map as M

import Data.Traversable ( forM )
import Control.Functor.HT ( void )


data Event = EventBool Name Bool
    deriving Show



moduleName :: Module.Name
moduleName = Module.Name "Controls"

changeControllerModule ::
    Program.Program ->
    Event ->
    Exc.Exceptional Exception.Message Program.Program
changeControllerModule p event = case event of
    EventBool name val ->
        flip Program.replaceModule p .
        Module.addRule ( controllerRule name val ) =<<
        Exc.fromMaybe
            ( Exception.Message Exception.InOut
                ( Program.dummyRange $ Module.deconsName moduleName )
                "cannot find module for controller updates" )
            ( M.lookup moduleName $ Program.modules p )

controllerRule ::
    Show a =>
    Name -> a -> Rule.Rule
controllerRule name val =
    Rule.Rule
        ( read "checkBox" )
        [ Term.StringLiteral
              ( Program.dummyRange $ Module.deconsName moduleName )
              ( deconsName name ),
          read "deflt" ]
        ( Term.Node ( read $ show val ) [] )

create ::
    WX.Frame () ->
    Assignments ->
    (Event -> IO ()) ->
    IO ()
create frame controls sink = do
    void $ WXCMZ.windowDestroyChildren frame
    panel <- WX.panel frame []
    ws <- forM (M.toList controls) $ \ ( name, (_rng, con) ) ->
      case con of
        CheckBox val -> do
            cb <- WX.checkBox panel
               [ text := deconsName name , checked := val ]
            set cb
               [ on command := do
                     c <- get cb checked
                     sink $ EventBool name c
               ]
            return $ widget cb
    set frame [ layout := container panel $ row 5 ws ]
