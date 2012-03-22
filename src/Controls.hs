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


data Event = EventBool Name Bool
    deriving Show

newtype Name = Name String
    deriving Show

deconsName :: Name -> String
deconsName (Name name) = name

data Control = CheckBox Bool

type Assignment = (Name, Control)


moduleName :: Module.Name
moduleName = Module.Name "Controls"

changeControllerModule ::
    Program.Program ->
    Controls.Event ->
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

controllerModule :: [ Assignment ] -> Module.Module
controllerModule controls =
    Module.fromDeclarations moduleName $ do
        ( name, CheckBox deflt ) <- controls
        return $ Module.RuleDeclaration
               $ controllerRule name deflt

create ::
    WX.Frame () ->
    [Assignment] ->
    (Controls.Event -> IO ()) ->
    IO ()
create frame controls sink = do
    void $ WXCMZ.windowDestroyChildren frame
    panel <- WX.panel frame []
    ws <- forM controls $ \ ( name, con ) ->
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


collect :: Program.Program -> [ Assignment ]
collect p = do
    ( _mod, contents ) <- M.toList $ Program.modules p
    Module.RuleDeclaration rule <- Module.declarations contents
    ( _pos, term ) <- Term.subterms $ Rule.rhs rule
    case Term.viewNode term of
        Just ( "checkBox" , [ Term.StringLiteral _rng tag, Term.Node val [] ] ) ->
              return ( Name tag
                     , CheckBox $ read ( Term.name val )
                     )
        _ -> []
