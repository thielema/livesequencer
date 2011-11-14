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
        connectTo, connectFrom :: Maybe String,
        httpPort :: Port
    }

newtype Port = Port { deconsPort :: Int }
    deriving (Eq, Show)

deflt :: Option
deflt =
    Option {
        moduleName = error "no module specified",
        importPaths = [ ".", "data", "data" </> "prelude" ],
        connectTo = Nothing,
        connectFrom = Nothing,
        httpPort = Port 8080
    }


parseNumber ::
   (Read a) =>
   String -> (a -> Bool) -> String -> String -> IO a
parseNumber name constraint constraintName str =
   case reads str of
      [(n, "")] ->
         if constraint n
           then return n
           else exitFailureMsg $ name ++ " must be a " ++ constraintName ++ " number"
      _ ->
         exitFailureMsg $ name ++ " must be a number, but is '" ++ str ++ "'"

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
    Opt.Option [] ["connect-from"]
        (flip ReqArg "ALSA-PORT" $ \str flags ->
            return $ flags{connectFrom = Just str})
        ("connect from an ALSA port at startup") :
    Opt.Option [] ["http-port"]
        (flip ReqArg "HTTP-PORT" $ \str flags ->
            fmap (\port -> flags{httpPort = Port $ fromInteger port}) $
            parseNumber "HTTP port" (\n -> 0<n && n<65536) "positive 16 bit" str)
        ("provide a web server under this port,\ndefault " ++
         (show $ deconsPort $ httpPort deflt) ) :
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
