module Option where

import Term ( Identifier )

import System.Environment ( getArgs )
import System.FilePath ( (</>) )

import qualified System.Exit as Exit
import qualified System.IO as IO


data Option = Option {
        moduleName :: Identifier,
        importPaths :: [FilePath]
    }

deflt :: Option
deflt =
    Option {
        moduleName = error "no module specified",
        importPaths = [ ".", "data", "data" </> "prelude" ]
    }


exitError :: String -> IO a
exitError msg = do
    IO.hPutStrLn IO.stderr msg
    Exit.exitFailure

get :: IO Option
get = do
    args <- getArgs
    case args of
        [] -> exitError "no module specified"
        _:_:_ -> exitError "more than one module specified"
        [modu] ->
            case reads modu of
                [(ident,"")] -> return $ deflt {moduleName = ident}
                _ -> exitError $ show modu ++ " is not a module name"
