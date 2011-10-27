-- |  controls are widgets that are:
-- * specified in the program text,
-- * displayed in the GUI,
-- * read while executing the program.

module Controls where

import qualified Program 
import qualified Module
import qualified Rule
import qualified Term

import qualified Data.Map as M
import Control.Monad ( forM )
import Graphics.UI.WX as WX

data Event = EventBool Term.Identifier Bool

data Control = CheckBox Bool

create frame panel prog sink = do
    ws <- forM ( collect prog ) $ \ ( name, con ) -> 
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
        ( "checkBox" , [ Term.String_Literal _ tag, Term.Node val [] ] ) -> 
              return ( Term.Identifier { Term.range = Term.range f
                                       , Term.name = Term.name mod      
                                             ++ "." ++ tag
                                       }
                     , CheckBox $ read ( Term.name val )
                     )
        _ -> []
