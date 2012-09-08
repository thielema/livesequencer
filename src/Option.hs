module Option where

import qualified Module
import qualified Time
import qualified IO
import Option.Utility ( exitFailureMsg, fmapOptDescr, parseNumber )
import qualified HTTPServer.Option as HTTP

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Paths_live_sequencer as Paths
import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt
          (getOpt, usageInfo, ArgDescr(NoArg, ReqArg), )
import System.Environment (getArgs, getProgName, )
import System.FilePath ( (</>), searchPathSeparator, isSearchPathSeparator, )

import System.Directory ( getCurrentDirectory )
import qualified System.Exit as Exit

import Control.Monad ( when )

import qualified Data.NonEmpty as NEList
import Data.Traversable ( forM )
import Data.Bool.HT ( if' )
import Data.List.HT ( chop )


data Option = Option {
        moduleNames :: [Module.Name],
        importPaths :: [FilePath],
        connect :: NEList.T [] Port,
        sequencerName :: String,
        latency :: Double,
        limits :: Limits,
        httpOption :: HTTP.Option
    }

-- the formatted value might look ugly
defltLatencyStr :: String
defltLatencyStr = "0.05"

getDeflt :: IO Option
getDeflt = do
    dataDir <- Paths.getDataDir
    curDir <- getCurrentDirectory
    return $
        Option {
            moduleNames = [],
            importPaths =
                curDir :
                map ((dataDir </>) . ("data" </>))
                    [ "prelude", "base", "example" ],
            connect = NEList.singleton (Port "inout" (Just []) (Just [])),
            sequencerName = "Rewrite-Sequencer",
            latency = read defltLatencyStr,
            limits = limitsDeflt,
            httpOption = HTTP.deflt
        }


data Port =
    Port {
        portName :: String,
        connectFrom, connectTo :: Maybe [String]
    }


data Limits =
    Limits {
        maxTermSize, maxTermDepth,
        maxReductions,
        maxEvents :: Int,
        eventPeriod :: Time.Milliseconds Integer
    }

limitsDeflt :: Limits
limitsDeflt = Limits {
        maxTermSize = 2000,
        maxTermDepth = 100,
        maxReductions = 1000,
        maxEvents = 150,
        eventPeriod = Time.milliseconds 1000
    }


{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
description :: Option -> [ Opt.OptDescr (Option -> IO Option) ]
description deflt =
    Opt.Option ['h'] ["help"]
        (NoArg $ \ _flags -> do
            programName <- getProgName
            putStrLn $
                usageInfo ("Usage: " ++ programName ++ " [OPTIONS] MODULE") $
                description deflt
            Exit.exitSuccess)
        "show options" :
    Opt.Option ['i'] ["import-paths"]
        (flip ReqArg "PATHS" $ \str flags ->
            return $ flags{importPaths =
               if null str
                 then []
                 else chop isSearchPathSeparator str ++ importPaths flags
            })
        ("if empty: clear import paths\n" ++
         "otherwise: add colon separated import paths,\n" ++
         "default:  " ++
         (case importPaths deflt of
            [] -> ""
            x:xs -> unlines $ x : map (("   "++) . (searchPathSeparator:)) xs)) :
    Opt.Option ['p'] ["connect-to"]
        (flip ReqArg "ADDRESS" $ \str flags ->
            case connect flags of
                NEList.Cons port ports ->
                    case connectTo port of
                        Just conns ->
                            return $ flags{connect = NEList.Cons
                                (port{connectTo = Just $ str : conns}) ports}
                        _ ->
                            exitFailureMsg $
                                "cannot connect to " ++ str ++
                                ", since port " ++ portName port ++ " does not allow output")
        ("connect to an ALSA port at startup,\n" ++
         "multiple connections per port are possible") :
    Opt.Option [] ["connect-from"]
        (flip ReqArg "ADDRESS" $ \str flags ->
            case connect flags of
                NEList.Cons port ports ->
                    case connectFrom port of
                        Just conns ->
                            return $ flags{connect = NEList.Cons
                                (port{connectFrom = Just $ str : conns}) ports}
                        _ ->
                            exitFailureMsg $
                                "cannot connect from " ++ str ++
                                ", since port " ++ portName port ++ " does not allow input")
        ("connect from an ALSA port at startup") :
    Opt.Option [] ["new-out-port"]
        (flip ReqArg "PORTNAME" $ \str flags ->
            return $ flags{connect =
                NEList.cons (Port str Nothing (Just [])) $
                connect flags})
        ("create new ALSA output port and add 16 MIDI channels") :
    Opt.Option [] ["sequencer-name"]
        (flip ReqArg "NAME" $ \str flags ->
            return $ flags{sequencerName = str})
        ("name of the ALSA sequencer client, default " ++
         sequencerName deflt) :
    Opt.Option [] ["latency"]
        (flip ReqArg "SECONDS" $ \str flags ->
            case reads str of
                [(x, "")] ->
                    if' (x<0)
                        (exitFailureMsg "latency must be non-negative") $
                    if' (x>1000)
                        (exitFailureMsg "latency is certainly too large") $
                    return $ flags{latency = x}
                _ -> exitFailureMsg "latency value must be a number")
        ("delay between evaluation and playing,\ndefault " ++
         defltLatencyStr) :
    map (fmapOptDescr $ \update old -> do
             newLimits <- update $ limits old
             return $ old {limits = newLimits})
        (limitsDescription (limits deflt)) ++
    map (fmapOptDescr $ \update old -> do
             newHTTP <- update $ httpOption old
             return $ old {httpOption = newHTTP})
        HTTP.description


limitsDescription :: Limits -> [ Opt.OptDescr (Limits -> IO Limits) ]
limitsDescription deflt =
    Opt.Option [] ["max-term-size"]
        (flip ReqArg "SIZE" $ \str flags ->
            fmap (\p -> flags{maxTermSize = fromInteger p}) $
            parseNumber "term size" (\n -> 0<n && n<1000000000) "positive 30 bit" str)
        ("maximum allowed term size, default " ++
         show (maxTermSize deflt)) :
    Opt.Option [] ["max-term-depth"]
        (flip ReqArg "SIZE" $ \str flags ->
            fmap (\p -> flags{maxTermDepth = fromInteger p}) $
            parseNumber "term depth" (\n -> 0<n && n<1000000000) "positive 30 bit" str)
        ("maximum allowed term depth, default " ++
         show (maxTermDepth deflt)) :
    Opt.Option [] ["max-reductions"]
        (flip ReqArg "NUMBER" $ \str flags ->
            fmap (\p -> flags{maxReductions = fromInteger p}) $
            parseNumber "number of reductions" (\n -> 0<n && n<1000000000) "positive 30 bit" str)
        ("maximum allowed reductions for every list element, default " ++
         show (maxReductions deflt)) :
    Opt.Option [] ["max-events-per-period"]
        (flip ReqArg "NUMBER" $ \str flags ->
            fmap (\p -> flags{maxEvents = fromInteger p}) $
            parseNumber "number of events" (\n -> 0<n && n<1000000000) "positive 30 bit" str)
        ("maximum number of allowed events per period, default " ++
         show (maxEvents deflt)) :
    Opt.Option [] ["event-period"]
        (flip ReqArg "MILLISECONDS" $ \str flags ->
            fmap (\p -> flags{eventPeriod = Time.milliseconds p}) $
            parseNumber "event period" (\n -> 0<n && n<1000000000) "positive 30 bit" str)
        ("period for limitting adjacent events, default " ++
         Time.format (eventPeriod deflt)) :
    []


get :: IO Option
get = do
    argv <- getArgs
    deflt <- getDeflt
    let (opts, files, errors) =
            getOpt Opt.RequireOrder (description deflt) argv
    when (not $ null errors) $
        exitFailureMsg (init (concat errors))

    dir <- getCurrentDirectory
    parsedOpts <-
        fmap (\o -> o { importPaths = map (dir </>) $ importPaths o } ) $
        foldl (>>=) (return deflt) opts

    names <-
        forM files $ \modu ->
            case Parsec.parse IO.input modu modu of
                Right name -> return name
                Left _ -> exitFailureMsg $ show modu ++ " is not a module name"
    return $ parsedOpts {
        connect = NEList.reverse $ connect parsedOpts,
        moduleNames = names
        }
