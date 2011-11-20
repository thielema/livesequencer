module HTTPServer.Option where

import Option.Utility ( parseNumber )

import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt (ArgDescr(..), )


data Option = Option {
        port :: Port
    }

newtype Port = Port { deconsPort :: Int }
    deriving (Eq, Show)

deflt :: Option
deflt =
    Option {
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
    []
