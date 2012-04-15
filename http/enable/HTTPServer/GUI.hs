module HTTPServer.GUI (
    HTTPServer.run,
    GuiUpdate,
    Feedback,
    update,
    methods,
    ) where

import qualified Module
import Utility.WX ( cursor )

import qualified HTTPServer

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX.Attributes ( Prop((:=)), set, get )
import Graphics.UI.WX.Classes ( text )
import qualified Control.Concurrent.Split.MVar as MVar

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class ( lift )

import qualified Data.Map as M


data GuiUpdate =
     GetModuleList { _moduleList :: MVar.In [ Module.Name ] }
   | GetModuleContent {
         _moduleName :: Module.Name,
         _moduleContent :: MVar.In (Exc.Exceptional HTTPServer.Error String) }
   | UpdateModuleContent {
         _moduleName :: Module.Name,
         _moduleEditableContent :: String,
         _moduleNewContent :: MVar.In Feedback }

type Feedback = Exc.Exceptional HTTPServer.Error (Maybe String, String)


methods :: (GuiUpdate -> IO ()) -> HTTPServer.Methods
methods output =
    HTTPServer.Methods {
        HTTPServer.getModuleList = do
            (modListIn,modListOut) <- MVar.newEmpty
            output $ GetModuleList modListIn
            MVar.take modListOut,
        HTTPServer.getModuleContent = \name -> Exc.ExceptionalT $ do
            (contentIn,contentOut) <- MVar.newEmpty
            output $ GetModuleContent name contentIn
            MVar.take contentOut,
        HTTPServer.updateModuleContent = \name edited -> Exc.ExceptionalT $ do
            (newContentIn,newContentOut) <- MVar.newEmpty
            output $ UpdateModuleContent name edited newContentIn
            MVar.take newContentOut
    }

update ::
    (MVar.In Feedback -> Module.Name -> String -> Int -> IO ()) ->
    WX.StatusField ->
    M.Map Module.Name (WX.TextCtrl ()) ->
    GuiUpdate ->
    IO ()
update input status editors req =
    case req of
        GetModuleList modList ->
            MVar.put modList . M.keys $ editors

        GetModuleContent name content ->
            (MVar.put content =<<) $ Exc.runExceptionalT $ do
                editor <- getModule editors name
                lift $ set status [ text :=
                    Module.tellName name ++ " downloaded by web client" ]
                lift $ get editor text

        UpdateModuleContent name content contentMVar -> do
            result <- Exc.runExceptionalT $ do
                editor <-
                    case M.lookup name editors of
                        Nothing ->
                            Exc.throwT
                                (Module.tellName name ++ " no longer available.",
                                 "")
                        Just pnl -> return pnl
                lift $ set status [ text :=
                    Module.tellName name ++ " updated by web client" ]
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
                    MVar.put contentMVar $
                        Exc.Success (Just e,
                            protected ++ HTTPServer.separatorLine ++ '\n' : content)
                Exc.Success (newContent, pos) ->
                    input contentMVar name newContent pos

getModule ::
    (Monad m) =>
    M.Map Module.Name a ->
    Module.Name ->
    Exc.ExceptionalT HTTPServer.Error m a
getModule pnls name =
    Exc.ExceptionalT $ return $
    Exc.fromMaybe
        (HTTPServer.notFound $ Module.tellName name ++ " not found") $
    M.lookup name pnls
