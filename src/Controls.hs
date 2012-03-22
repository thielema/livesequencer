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
import Data.Traversable ( forM )
import Control.Functor.HT ( void )


data Event = EventBool Name Bool
    deriving Show

data Control = CheckBox Bool

type Assignments = M.Map Name Control


newtype Name = Name String
    deriving (Eq, Ord, Show)

deconsName :: Name -> String
deconsName (Name name) = name



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

controllerModule :: Assignments -> Module.Module
controllerModule controls =
    Module.fromDeclarations moduleName $ do
        ( name, CheckBox deflt ) <- M.toList controls
        return $ Module.RuleDeclaration
               $ controllerRule name deflt

create ::
    WX.Frame () ->
    Assignments ->
    (Controls.Event -> IO ()) ->
    IO ()
create frame controls sink = do
    void $ WXCMZ.windowDestroyChildren frame
    panel <- WX.panel frame []
    ws <- forM (M.toList controls) $ \ ( name, con ) ->
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


exc :: Term.Range -> String -> Exception.Message
exc rng msg =
    Exception.Message Exception.Parse rng msg

collect ::
    Program.Program ->
    Exc.Exceptional Exception.Message Assignments
collect p =
    flip (foldr
        (\ea go m -> do
            (name, (rng, ctrl)) <- ea
            Exc.assert
                (exc rng $
                 "duplicate controller definition with name "
                  ++ deconsName name) $ not $ M.member name m
            go (M.insert name ctrl m))
        return) M.empty $ do

    content <- M.elems $ Program.modules p
    Module.RuleDeclaration rule <- Module.declarations content
    ( _pos, term ) <- Term.subterms $ Rule.rhs rule
    case Term.viewNode term of
        Just ( "checkBox" , args ) ->
            return $
            case args of
                [ Term.StringLiteral _rng tag, Term.Node val [] ] ->
                    case reads $ Term.name val of
                        [(b, "")] ->
                            Exc.Success $ (Name tag, (Term.termRange term, CheckBox b))
                        _ ->
                            Exc.Exception $
                            exc (Term.range val) $
                            "cannot parse Bool value " ++
                            show (Term.name val) ++ " for getBox"
                _ ->
                    Exc.Exception $
                    exc (Term.termRange term) "invalid checkBox arguments"
        _ -> []
