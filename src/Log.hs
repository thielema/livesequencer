module Log where

import qualified System.IO as IO
import Control.Monad ( when )


toConsole :: Bool
toConsole = False

put :: String -> IO ()
put msg = when toConsole $ IO.hPutStrLn IO.stderr msg
