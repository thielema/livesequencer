module HTTPServer (
    Methods(Methods),
    run,
    getModuleList,
    getModuleContent,
    updateModuleContent,
    separatorLine,
    splitProtected,
    Error, notFound,
    ) where

import qualified HTTPServer.Option as Option
import qualified Module
import qualified IO

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
import Control.Functor.HT ( void )


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
        getModuleList :: IO [Module.Name],
        getModuleContent ::
            Module.Name -> ExceptionalT Error IO String,
        updateModuleContent ::
            Module.Name -> String ->
            ExceptionalT Error IO (Maybe String, String)
    }

run :: Methods -> Option.Option -> IO ()
run dict opt = 
    case Option.port opt of
        Option.Port port ->
            void $ HTTPd.initServer port $
                handleException . server dict opt

server ::
    Methods ->
    Option.Option ->
    HTTPd.Request ->
    ExceptionalT Error IO HTTPd.Response
server dict opt req =
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
                        formatModuleContent opt modList modIdent (Nothing, content)
                _ ->
                    Exc.throwT $ badRequest $ "Bad path in URL"
        "POST" ->
            case uriPath $ HTTPd.reqURI req of
                '/':modName -> do
                    modList <- MT.lift $ getModuleList dict
                    modIdent <- parseModuleName modName
                    editable <-
                        case lookup "content" $ CGI.formDecode $ HTTPd.reqBody req of
                            Just str -> return str
                            _ -> Exc.throwT $ badRequest $
                                 "Argument 'content' missing"
                    updatedContent <-
                        updateModuleContent dict modIdent editable
                    return $ HTTPd.Response 200 headers $
                        formatModuleContent opt modList modIdent updatedContent
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
    String -> ExceptionalT Error m Module.Name
parseModuleName modName =
    Exc.mapExceptionT
        (badRequest .
         ("syntax error in module name:\n"++) .
         show) $
    Exc.fromEitherT $ return $
    Parsec.parse
        (Parsec.between (return ()) Parsec.eof IO.input)
        "" modName

formatModuleList :: [Module.Name] -> String
formatModuleList list =
    Html.renderHtml $
    Html.header (Html.thetitle << "Haskell Live Sequencer - Module list") +++
    Html.body (htmlFromModuleList list)

formatModuleContent ::
    Option.Option ->
    [Module.Name] -> Module.Name -> (Maybe String, String) -> String
formatModuleContent opt list name (mmsg, content) =
    Html.renderHtml $
    Html.header (Html.thetitle <<
        ("Haskell Live Sequencer - " ++ Module.tellName name)) +++
    (Html.body $
        sideBySide
            (htmlFromModuleList list)
            (Html.h1 << Module.deconsName name +++
             maybe Html.noHtml
                 (\msg -> (Html.! [Html.color Html.red]) $ Html.font $
                     htmlFromMultiline $ "error:\n" ++ msg) mmsg +++
             ((Html.! [Html.action $ Module.deconsName name, Html.method "post",
                       Html.HtmlAttr "accept-charset" "ISO-8859-1"]) $
              Html.form $
                  case splitProtected content of
                      (protected, sepEditable) ->
                          Html.pre << protected
                          +++
                          case sepEditable of
                              Nothing -> Html.noHtml
                              Just (separator, editable) ->
                                  Html.pre << separator
                                  +++
                                  Html.textarea
                                      Html.! [Html.name "content",
                                              Html.rows $ Option.rows opt,
                                              Html.cols $ Option.columns opt]
                                      << editable
                                  +++
                                  Html.br
                                  +++
                                  Html.submit "" "submit")))

splitProtected :: String -> (String, Maybe (String, String))
splitProtected =
    mapPair (unlines,
        \lns ->
            case lns of
                separator:suffix -> Just (separator, unlines suffix)
                [] -> Nothing) .
    ListHT.break (List.isPrefixOf separatorLine) .
    lines

separatorLine :: String
separatorLine = replicate 8 '-'

htmlFromMultiline :: String -> Html.Html
htmlFromMultiline =
    Html.concatHtml . List.intersperse Html.br .
    map Html.toHtml . lines

{-
As far as I can see,
CSS would require me to use an absolute horizontal position
for the module content.
Thus I stick to the much-maligned tables.
-}
sideBySide :: Html.Html -> Html.Html -> Html.Html
sideBySide left right =
    Html.simpleTable [] [Html.valign "top"] [[left, right]]

htmlFromModuleList :: [Module.Name] -> Html.Html
htmlFromModuleList =
    (Html.! [Html.identifier "module-list"]) .
    Html.unordList . map
       (\name -> (Html.anchor << Module.deconsName name) Html.! [Html.href $ Module.deconsName name])
