module HTTPServer.Option where

import qualified System.Console.GetOpt as Opt


data Option = Option

deflt :: Option
deflt = Option

description :: [Opt.OptDescr (Option -> IO Option)]
description = []
