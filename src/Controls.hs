-- |  controls are widgets that are:
-- * specified in the program text,
-- * displayed in the GUI,
-- * read while executing the program.

module Controls where

import qualified Program
import qualified Module
import qualified Rule
import qualified Term
import qualified Exception

import qualified Control.Monad.Exception.Synchronous as Exc

import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore.WxcClassesMZ as WXCMZ
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events
import Graphics.UI.WX.Layout ( layout, container, row, widget )

import qualified Data.Map as M
import Control.Monad ( forM )
import Control.Functor.HT ( void )


data Event = EventBool Term.Identifier Bool
    deriving Show

data Control = CheckBox Bool


moduleName :: Module.Name
moduleName = Module.Name "Controls"

get_controller_module :: Program.Program -> Module.Module
get_controller_module p =
    let Just m = M.lookup moduleName $ Program.modules p
    in  m

change_controller_module ::
    Program.Program ->
    Controls.Event ->
    Exc.Exceptional Exception.Message Program.Program
change_controller_module p event = case event of
    EventBool name val ->
        flip Program.add_module p $
        Module.add_rule ( controller_rule name val ) $
        get_controller_module p

controller_rule ::
    Show a =>
    Term.Identifier -> a -> Rule.Rule
controller_rule name val =
    Rule.Rule
        ( read "checkBox" )
        [ Term.Node name [], read "deflt" ]
        ( Term.Node ( read $ show val ) [] )

controller_module :: [(Term.Identifier, Control)] -> Module.Module
controller_module controls =
    let decls = do
            ( name, CheckBox deflt ) <- controls
            return $ Module.Rule_Declaration
                   $ controller_rule name deflt
        m = Module.Module { Module.name = moduleName
                 , Module.imports = []
                 , Module.source_text = show m
                 , Module.source_location = "/dev/null"
                 , Module.functions =
                      Module.make_functions decls
                 , Module.declarations = decls
                 }
    in  m

create ::
    WX.Frame b ->
    [(Term.Identifier, Control)] ->
    (Controls.Event -> IO ()) ->
    IO ()
create frame controls sink = do
    void $ WXCMZ.windowDestroyChildren frame
    panel <- WX.panel frame []
    ws <- forM controls $ \ ( name, con ) ->
      case con of
        CheckBox val -> do
            cb <- WX.checkBox panel
               [ text := Term.name name , checked := val ]
            set cb
               [ on command := do
                     c <- get cb checked
                     sink $ EventBool name c
               ]
            return $ widget cb
    set frame [ layout := container panel $ row 5 ws ]


collect :: Program.Program -> [ ( Term.Identifier, Control ) ]
collect p = do
    ( _mod, contents ) <- M.toList $ Program.modules p
    Module.Rule_Declaration rule <- Module.declarations contents
    ( _pos, term ) <- Term.subterms $ Rule.rhs rule
    case Term.viewNode term of
        Just ( "checkBox" , [ Term.Node tag [], Term.Node val [] ] ) ->
              return ( tag
                     , CheckBox $ read ( Term.name val )
                     )
        _ -> []
