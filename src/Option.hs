module Option where

import Term ( Identifier )

import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt
          (getOpt, ArgOrder(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.FilePath ( (</>), searchPathSeparator )

import System.Directory ( getCurrentDirectory )
import qualified System.Exit as Exit
import qualified System.IO as IO

import Control.Monad ( when )

import Data.List.HT ( chop )
import Data.List ( intercalate )


data Option = Option {
        moduleName :: Identifier,
        importPaths :: [FilePath],
        connectTo :: Maybe String
    }

deflt :: Option
deflt =
    Option {
        moduleName = error "no module specified",
        importPaths = [ ".", "data", "data" </> "prelude" ],
        connectTo = Nothing
    }


exitFailureMsg :: String -> IO a
exitFailureMsg msg = do
    IO.hPutStrLn IO.stderr msg
    Exit.exitFailure

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
         intercalate ":" (importPaths deflt)) :
    Opt.Option ['p'] ["connect-to"]
        (flip ReqArg "ALSA-PORT" $ \str flags ->
            return $ flags{connectTo = Just str})
        ("connect to an ALSA port at startup") :
    []


get :: IO Option
get = do
    argv <- getArgs
    let (opts, files, errors) = getOpt RequireOrder description argv
    when (not $ null errors) $
        exitFailureMsg (init (concat errors))

    dir <- getCurrentDirectory
    parsedOpts <-
        fmap (\o -> o { importPaths = map (dir </>) $ importPaths o } ) $
        foldl (>>=) (return deflt) opts

    case files of
        [] -> exitFailureMsg "no module specified"
        _:_:_ -> exitFailureMsg "more than one module specified"
        [modu] ->
            case reads modu of
                [(ident,"")] -> return $ parsedOpts {moduleName = ident}
                _ -> exitFailureMsg $ show modu ++ " is not a module name"
