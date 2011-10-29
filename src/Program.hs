module Program where

import IO
import Term ( Identifier (..) )
import Rule
import Module

import Text.ParserCombinators.Parsec ( parse )

import System.Directory ( doesFileExist )
import System.IO ( hPutStrLn, stderr )
import System.FilePath ( (</>) )

import qualified Data.Map as M
import Control.Monad ( foldM )


data Program = Program { modules :: M.Map Identifier Module }
    deriving (Show)

add_module :: Program -> Module -> Program
add_module p m = p { modules = M.insert ( Module.name m ) m $ modules p }

rules :: Program -> [ Rule ]
rules p = concat $ map Module.rules $ M.elems $ modules p

-- | load from disk, with import chasing
chase :: [ FilePath ] -> Identifier -> IO Program
chase dirs n = chaser dirs ( Program { modules = M.empty } )  n

-- FIXME: errors should be exceptions in order to handle failed load at runtime
chaser :: [ FilePath ] -> Program -> Identifier -> IO Program
chaser dirs p n = do
    hPutStrLn stderr $ unwords [ "chasing", "module", show n ]
    case M.lookup n ( modules p ) of
        Just _ -> do
            hPutStrLn stderr $ "module is already loaded"
            return p
        Nothing -> do
            let f = map ( \ c -> if c == '.' then '/' else c ) ( Term.name n )
                  ++ ".hs"
            ff <- chaseFile dirs f
            s <- readFile ff
            case parse input ( Term.name n ) s of
                Left err -> error $ show err
                Right m0  -> do
                    let m = m0 { source_location = ff, source_text = s }
                    hPutStrLn stderr $ show m
                    foldM ( chaser dirs )
                          ( p { modules = M.insert n m $ modules p } )
                          $ map source $ imports m

-- | look for file, trying to append its name to the directories in the path,
-- in turn. Will fail if file is not found.
-- FIXME: Opening the file may fail nevertheless, thus we should merge chasing and opening files.
chaseFile :: [FilePath] -> FilePath -> IO String
chaseFile [] f = do
    error $ unwords [ "module", "not", "found:", f ]
chaseFile (dir:dirs) f = do
    let ff = dir </> f
    e <- doesFileExist ff
    if e then do
           hPutStrLn stderr $ unwords [ "found at location", ff ]
           return ff
         else chaseFile dirs f
