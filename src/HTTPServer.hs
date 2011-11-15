module HTTPServer where

import Term ( Identifier )
import qualified IO
import qualified Option

import qualified Network.Shed.Httpd as HTTPd
import Network.URI ( uriPath )

import Text.Html((<<), (+++), )
import qualified Text.Html as Html

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Exception.Synchronous ( ExceptionalT )

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Control.Monad.Trans.Class as MT

import qualified Data.List.HT as ListHT
import qualified Data.List as List
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
            Identifier -> ExceptionalT Error IO String
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
                        modIdent <-
                            Exc.mapExceptionT
                                (badRequest .
                                 ("syntax error in module name:\n"++) .
                                 show) $
                            Exc.fromEitherT $ return $
                            Parsec.parse
                                (Parsec.between (return ()) Parsec.eof IO.input)
                                "" modName
                        content <- getModuleContent dict modIdent
                        return $ HTTPd.Response 200 headers $
                            formatModuleContent modList modIdent content
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


formatModuleList :: [Identifier] -> String
formatModuleList list =
    Html.renderHtml $
    Html.header (Html.thetitle << "Haskell Live Sequencer - Module list") +++
    Html.body (htmlFromModuleList list)

formatModuleContent :: [Identifier] -> Identifier -> String -> String
formatModuleContent list name content =
    Html.renderHtml $
    Html.header (Html.thetitle <<
        ("Haskell Live Sequencer - Module " ++ show name)) +++
    (Html.body $
        htmlFromModuleList list +++
        Html.h1 << show name +++
        ((Html.! [Html.action $ show name]) $
         Html.form $
             (let (protected,editable) =
                      ListHT.breakAfter
                          (List.isPrefixOf $ replicate 8 '-') $
                      lines content
              in  Html.pre << unlines protected
                  +++
                  -- Html.hr +++
                  Html.textarea
                      Html.! [Html.name $ show name, Html.rows "30", Html.cols "100"]
                      << unlines editable)
             +++
             Html.br
             +++
             Html.submit "" "submit"))

htmlFromModuleList :: [Identifier] -> Html.Html
htmlFromModuleList =
    Html.unordList . map
       (\name -> (Html.anchor << show name) Html.! [Html.href $ show name])
