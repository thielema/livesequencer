module HTTPServer where

import Term ( Identifier )
import qualified IO
import qualified Option

import qualified Network.Shed.Httpd as HTTPd
import qualified Network.CGI as CGI
import Network.URI ( uriPath )

import Text.Html((<<), (+++), )
import qualified Text.Html as Html

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Exception.Synchronous ( ExceptionalT )

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Control.Monad.Trans.Class as MT

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT ( mapPair )
import Utility ( void )


headers :: [(String, String)]
headers =
    [("Content-Type", "text/html; charset=latin1")]

data Error = Error Int String

badRequest :: String -> Error
badRequest = Error 400

notFound :: String -> Error
notFound = Error 404

methodNotAllowed :: String -> Error
methodNotAllowed = Error 405


data Methods =
    Methods {
        getModuleList :: IO [Identifier],
        getModuleContent ::
            Identifier -> ExceptionalT Error IO String,
        updateModuleContent ::
            Identifier -> String ->
            ExceptionalT Error IO (Maybe String, String)
    }

run :: Methods -> Option.Port -> IO ()
run dict (Option.Port port) =
    void $ HTTPd.initServer port $ \req ->
        handleException $
        case HTTPd.reqMethod req of
            "GET" ->
                case uriPath (HTTPd.reqURI req) of
                    "/" ->
                        MT.lift $
                        fmap (HTTPd.Response 200 headers . formatModuleList) $
                        getModuleList dict
                    '/':modName -> do
                        modList <- MT.lift $ getModuleList dict
                        modIdent <- parseModuleName modName
                        content <- getModuleContent dict modIdent
                        return $ HTTPd.Response 200 headers $
                            formatModuleContent modList modIdent (Nothing, content)
                    _ ->
                        Exc.throwT $ badRequest $ "Bad path in URL"
            "POST" ->
                case uriPath $ HTTPd.reqURI req of
                    '/':modName -> do
                        modList <- MT.lift $ getModuleList dict
                        modIdent <- parseModuleName modName
                        editable <-
                            case CGI.formDecode $ HTTPd.reqBody req of
                                [("content", str)] -> return str
                                _ -> Exc.throwT $ badRequest $
                                     "The only argument must be 'content'"
                        updatedContent <-
                            updateModuleContent dict modIdent editable
                        return $ HTTPd.Response 200 headers $
                            formatModuleContent modList modIdent updatedContent
                    _ ->
                        Exc.throwT $ badRequest $ "Bad path in URL"
            method ->
                Exc.throwT $ methodNotAllowed $
                    "Method " ++ method ++ " not allowed"


handleException ::
    (Monad m) =>
    ExceptionalT Error m HTTPd.Response -> m HTTPd.Response
handleException =
    Exc.resolveT $ \(Error errorCode msg) ->
        return $
        HTTPd.Response errorCode headers $
        Html.renderHtml $
        Html.header (Html.thetitle <<
            ("Haskell Live Sequencer - Error " ++ show errorCode)) +++
        (Html.body $ Html.concatHtml $
         map (\line -> Html.toHtml line +++ Html.br) $ lines msg)


parseModuleName ::
    (Monad m) =>
    String -> ExceptionalT Error m Identifier
parseModuleName modName =
    Exc.mapExceptionT
        (badRequest .
         ("syntax error in module name:\n"++) .
         show) $
    Exc.fromEitherT $ return $
    Parsec.parse
        (Parsec.between (return ()) Parsec.eof IO.input)
        "" modName

formatModuleList :: [Identifier] -> String
formatModuleList list =
    Html.renderHtml $
    Html.header (Html.thetitle << "Haskell Live Sequencer - Module list") +++
    Html.body (htmlFromModuleList list)

formatModuleContent ::
    [Identifier] -> Identifier -> (Maybe String, String) -> String
formatModuleContent list name (mmsg, content) =
    Html.renderHtml $
    Html.header (Html.thetitle <<
        ("Haskell Live Sequencer - Module " ++ show name)) +++
    (Html.body $
        sideBySide
            (htmlFromModuleList list)
            (Html.h1 << show name +++
             Html.toHtml (maybe "" (\msg -> "error: " ++ msg) mmsg) +++
             ((Html.! [Html.action $ show name, Html.method "post",
                       Html.HtmlAttr "accept-charset" "ISO-8859-1"]) $
              Html.form $
                  case splitProtected content of
                       (protected,editable) ->
                           Html.pre << protected
                           +++
                           if null editable
                             then Html.noHtml
                             else
                               Html.textarea
                                   Html.! [Html.name "content",
                                           Html.rows "30", Html.cols "100"]
                                   << editable
                               +++
                               Html.br
                               +++
                               Html.submit "" "submit")))

splitProtected :: String -> (String, String)
splitProtected =
    mapPair (unlines, unlines) .
    ListHT.breakAfter
        (List.isPrefixOf $ replicate 8 '-') .
    lines


{-
As far as I can see,
CSS would require me to use an absolute horizontal position
for the module content.
Thus I stick to the much-maligned tables.
-}
sideBySide :: Html.Html -> Html.Html -> Html.Html
sideBySide left right =
    Html.simpleTable [] [Html.valign "top"] [[left, right]]

htmlFromModuleList :: [Identifier] -> Html.Html
htmlFromModuleList =
    (Html.! [Html.identifier "module-list"]) .
    Html.unordList . map
       (\name -> (Html.anchor << show name) Html.! [Html.href $ show name])
