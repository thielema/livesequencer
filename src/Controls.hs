-- |  controls are widgets that are:
-- * specified in the program text,
-- * displayed in the GUI,
-- * read while executing the program.

module Controls where

import qualified Program
import qualified Module
import qualified Rule
import qualified Term

import qualified Text.ParserCombinators.Parsec.Pos as Pos

import qualified Data.Map as M
import Control.Monad ( forM )
import Graphics.UI.WX as WX

data Event = EventBool Term.Identifier Bool
    deriving Show

data Control = CheckBox Bool


get_controller_module p = 
        let Just m = M.lookup ( read "Controls" ) $ Program.modules p
        in  m

change_controller_module p event = case event of
    EventBool name val -> 
        let m = get_controller_module p
            m' = Module.add_rule m $ controller_rule name val
        in  Program.add_module p m'

controller_rule name val =
    Rule.Rule
        ( read "checkBox" )
        [ Term.Node name [], read "deflt" ]
        ( Term.Node ( read $ show val ) [] )

controller_module controls =
    let decls = do
            ( name, CheckBox deflt ) <- controls
            return $ Module.Rule_Declaration
                   $ controller_rule name deflt
        m = Module.Module { Module.name = read "Controls"
                 , Module.imports = []
                 , Module.source_text = show m
                 , Module.source_location = "/dev/null"
                 , Module.function_declarations =
                      Module.make_function_declarations decls
                 , Module.declarations = decls
                 }
    in  m

create frame panel controls sink = do
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
    ( mod, contents ) <- M.toList $ Program.modules p
    Module.Rule_Declaration rule <- Module.declarations contents
    ( pos, t @ ( Term.Node f args ) ) <- Term.subterms $ Rule.rhs rule
    case ( Term.name f, args ) of
        ( "checkBox" , [ Term.Node tag [], Term.Node val [] ] ) -> 
              return ( tag
                     , CheckBox $ read ( Term.name val )
                     )
        _ -> []
