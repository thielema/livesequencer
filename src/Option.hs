module Option where

import qualified Module
import qualified IO
import Option.Utility ( exitFailureMsg, fmapOptDescr )
import qualified HTTPServer.Option as HTTP

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Paths_live_sequencer as Paths
import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt
          (getOpt, ArgOrder(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.FilePath ( (</>), searchPathSeparator )

import System.Directory ( getCurrentDirectory )
import qualified System.Exit as Exit

import Control.Monad ( when )

import Data.List.HT ( chop )
import Data.List ( intercalate )


data Option = Option {
        moduleName :: Module.Name,
        importPaths :: [FilePath],
        connectTo, connectFrom :: Maybe String,
        httpOption :: HTTP.Option
    }

{-
These are the paths that might be used for tests without installation.
-}
defltPaths :: [ FilePath ]
defltPaths = [ "data", "data" </> "prelude" ]

deflt :: IO Option
deflt = do
    dataDir <- Paths.getDataDir
    return $
        Option {
            moduleName = error "no module specified",
            importPaths = map (dataDir </>) defltPaths,
            connectTo = Nothing,
            connectFrom = Nothing,
            httpOption = HTTP.deflt
        }


{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
description :: [Opt.OptDescr (Option -> IO Option)]
description =
    Opt.Option ['h'] ["help"]
        (NoArg $ \ _flags -> do
            programName <- getProgName
            putStrLn $
                usageInfo ("Usage: " ++ programName ++ " [OPTIONS]") description
            Exit.exitSuccess)
        "show options" :
    Opt.Option ['i'] ["import-paths"]
        (flip ReqArg "PATHS" $ \str flags ->
            return $ flags{importPaths = chop (searchPathSeparator==) str})
        ("colon separated import paths,\ndefault " ++
         intercalate ":" defltPaths) :
    Opt.Option ['p'] ["connect-to"]
        (flip ReqArg "ALSA-PORT" $ \str flags ->
            return $ flags{connectTo = Just str})
        ("connect to an ALSA port at startup") :
    Opt.Option [] ["connect-from"]
        (flip ReqArg "ALSA-PORT" $ \str flags ->
            return $ flags{connectFrom = Just str})
        ("connect from an ALSA port at startup") :
    map (fmapOptDescr $ \update old -> do
             newHTTP <- update $ httpOption old
             return $ old {httpOption = newHTTP})
        HTTP.description


get :: IO Option
get = do
    argv <- getArgs
    let (opts, files, errors) = getOpt RequireOrder description argv
    when (not $ null errors) $
        exitFailureMsg (init (concat errors))

    dir <- getCurrentDirectory
    parsedOpts <-
        fmap (\o -> o { importPaths = map (dir </>) $ importPaths o } ) $
        foldl (>>=) deflt opts

    case files of
        [] -> exitFailureMsg "no module specified"
        _:_:_ -> exitFailureMsg "more than one module specified"
        [modu] ->
            case Parsec.parse IO.input "" modu of
                Right name -> return $ parsedOpts {moduleName = name}
                Left _ -> exitFailureMsg $ show modu ++ " is not a module name"
