module Option.Utility where

import qualified System.Console.GetOpt as G
import qualified System.Exit as Exit
import qualified System.IO as IO


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

fmapArgDescr :: (a -> b) -> (G.ArgDescr a -> G.ArgDescr b)
fmapArgDescr f d =
    case d of
        G.NoArg a -> G.NoArg $ f a
        G.ReqArg g str -> G.ReqArg (f.g) str
        G.OptArg g str -> G.OptArg (f.g) str

fmapOptDescr :: (a -> b) -> (G.OptDescr a -> G.OptDescr b)
fmapOptDescr f (G.Option short long arg help) =
    G.Option short long (fmapArgDescr f arg) help
