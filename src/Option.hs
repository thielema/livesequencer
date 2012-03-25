module Option where

import qualified Module
import qualified IO
import Option.Utility ( exitFailureMsg, fmapOptDescr )
import qualified HTTPServer.Option as HTTP

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Paths_live_sequencer as Paths
import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt
          (getOpt, usageInfo, ArgDescr(NoArg, ReqArg), )
import System.Environment (getArgs, getProgName, )
import System.FilePath ( (</>), searchPathSeparator )

import System.Directory ( getCurrentDirectory )
import qualified System.Exit as Exit

import Control.Monad ( when )

import qualified Utility.NonEmptyList as NEList
import Data.Traversable ( forM )
import Data.List.HT ( chop )
import Data.List ( intercalate )


data Option = Option {
        moduleNames :: [Module.Name],
        importPaths :: [FilePath],
        connect :: NEList.T Port,
        sequencerName :: String,
        httpOption :: HTTP.Option
    }

getDeflt :: IO Option
getDeflt = do
    dataDir <- Paths.getDataDir
    return $
        Option {
            moduleNames = [],
            importPaths = map (dataDir </>) [ "data", "data" </> "prelude" ],
            connect = NEList.singleton (Port "inout" (Just []) (Just [])),
            sequencerName = "Rewrite-Sequencer",
            httpOption = HTTP.deflt
        }


data Port =
    Port {
        portName :: String,
        connectFrom, connectTo :: Maybe [String]
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
            return $ flags{importPaths = chop (searchPathSeparator==) str})
        ("colon separated import paths,\ndefault " ++
         intercalate ":" (importPaths deflt)) :
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
        ("name of the ALSA sequencer client,\ndefault " ++
         sequencerName deflt) :
    map (fmapOptDescr $ \update old -> do
             newHTTP <- update $ httpOption old
             return $ old {httpOption = newHTTP})
        HTTP.description


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
