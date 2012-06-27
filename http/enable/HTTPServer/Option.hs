module HTTPServer.Option where

import Option.Utility ( parseNumber )

import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt (ArgDescr(..), )


data Option = Option {
        rows, columns :: String,
        port :: Port
    }

newtype Port = Port { deconsPort :: Int }
    deriving (Eq, Show)

deflt :: Option
deflt =
    Option {
        rows = "30", columns = "100",
        port = Port 8080
    }


description :: [Opt.OptDescr (Option -> IO Option)]
description =
    Opt.Option [] ["http-port"]
        (flip ReqArg "HTTP-PORT" $ \str flags ->
            fmap (\p -> flags{port = Port $ fromInteger p}) $
            parseNumber "HTTP port" (\n -> 0<n && n<65536) "positive 16 bit" str)
        ("provide a web server under this port,\ndefault " ++
         (show $ deconsPort $ port deflt) ) :
    Opt.Option [] ["html-rows"]
        (flip ReqArg "NUMBER" $ \str flags ->
            return $ flags{rows = str})
        ("number of rows in the text area for the module content,\ndefault " ++
         rows deflt) :
    Opt.Option [] ["html-columns"]
        (flip ReqArg "NUMBER" $ \str flags ->
            return $ flags{columns = str})
        ("number of columns in the text area for the module content,\ndefault " ++
         columns deflt) :
    []
