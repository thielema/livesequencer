module HTTPServer.GUI (
    HTTPServer.run,
    GuiUpdate,
    Feedback,
    update,
    methods,
    ) where

import Term ( Identifier )
import Utility.WX ( cursor )

import qualified HTTPServer

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Controls
import Control.Concurrent.MVar

import Data.IORef ( IORef, readIORef )

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class ( lift )

import qualified Data.Map as M


data GuiUpdate =
     GetModuleList { _moduleList :: MVar [ Identifier ] }
   | GetModuleContent {
         _moduleName :: Identifier,
         _moduleContent :: MVar (Exc.Exceptional HTTPServer.Error String) }
   | UpdateModuleContent {
         _moduleName :: Identifier,
         _moduleEditableContent :: String,
         _moduleNewContent :: MVar Feedback }

type Feedback = Exc.Exceptional HTTPServer.Error (Maybe String, String)


methods :: (GuiUpdate -> IO ()) -> HTTPServer.Methods
methods output =
    HTTPServer.Methods {
        HTTPServer.getModuleList = do
            modList <- newEmptyMVar
            output $ GetModuleList modList
            takeMVar modList,
        HTTPServer.getModuleContent = \name -> Exc.ExceptionalT $ do
            content <- newEmptyMVar
            output $ GetModuleContent name content
            takeMVar content,
        HTTPServer.updateModuleContent = \name edited -> Exc.ExceptionalT $ do
            newContent <- newEmptyMVar
            output $ UpdateModuleContent name edited newContent
            takeMVar newContent
    }

update ::
    (MVar Feedback -> Identifier -> String -> Int -> IO ()) ->
    WX.StatusField ->
    IORef (M.Map Identifier (a, TextCtrl b, c)) ->
    GuiUpdate ->
    IO ()
update input status panels req =
    case req of
        GetModuleList modList ->
            putMVar modList . M.keys =<< readIORef panels

        GetModuleContent name content ->
            (putMVar content =<<) $ Exc.runExceptionalT $ do
                pnls <- lift $ readIORef panels
                (_,editor,_) <- getModule pnls name
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
                    input contentMVar name newContent pos

getModule ::
    (Monad m) =>
    M.Map Identifier a ->
    Identifier ->
    Exc.ExceptionalT HTTPServer.Error m a
getModule pnls name =
    Exc.ExceptionalT $ return $
    Exc.fromMaybe
        (HTTPServer.notFound $ "module " ++ show name ++ " not found") $
    M.lookup name pnls
