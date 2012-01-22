module HTTPServer.GUI (
    run,
    GuiUpdate,
    Feedback,
    update,
    methods,
    ) where

import qualified HTTPServer.Option as Option

import qualified Module

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Controls ( TextCtrl )
import Control.Concurrent.MVar

import Data.IORef ( IORef )

import qualified Control.Monad.Exception.Synchronous as Exc

import qualified Data.Map as M


-- custom 'data' would be nicer, but it yields warning about unused constructor
-- EmptyDataDecls would be a way out
type GuiUpdate = ()

type Error = ()

data Methods = Methods

type Feedback = Exc.Exceptional Error (Maybe String, String)

run :: Methods -> Option.Option -> IO ()
run _dict _opt = return ()

methods :: (GuiUpdate -> IO ()) -> Methods
methods _output = Methods

update ::
    (MVar Feedback -> Module.Name -> String -> Int -> IO ()) ->
    WX.StatusField ->
    IORef (M.Map Module.Name (a, TextCtrl b, c)) ->
    GuiUpdate ->
    IO ()
update _input _status _panels _req = return ()