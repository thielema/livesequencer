module Program where

import IO
import Term ( Range (start), Identifier (..) )
import Module ( Module )
import qualified Module

import qualified Control.Monad.Exception.Synchronous as Exc

import Text.ParserCombinators.Parsec ( parse )
import qualified Text.ParserCombinators.Parsec.Pos as Pos

import System.Directory ( doesFileExist )
import System.IO ( hPutStrLn, stderr )
import System.FilePath ( (</>) )

import qualified Data.Traversable as Trav
import qualified Data.Map as M
import Control.Monad ( foldM )


data Program = Program
     { modules :: M.Map Identifier Module
     , functions :: Module.FunctionDeclarations
     }
    deriving (Show)

add_module :: Module -> Program -> Program
add_module m p =
    let funcs =
            M.difference
                (functions p)
                (M.findWithDefault M.empty ( Module.name m )
                    ( fmap Module.functions $ modules p ))
    in  p { modules = M.insert ( Module.name m ) m $ modules p,
            functions =
                Exc.resolve (error . show) $
                union_functions funcs $ Module.functions m }

union_functions ::
    Module.FunctionDeclarations ->
    Module.FunctionDeclarations ->
    Exc.Exceptional (Range, String) Module.FunctionDeclarations
union_functions m0 m1 =
    let f = M.mapWithKey (\nm rs -> (nm, Exc.Success rs))
    in  Trav.sequenceA . fmap snd $
        M.unionWith (\(n0,_) (n1,_) ->
            (n0,
             Exc.Exception
                 (range n0,
                  "duplicate definition of " ++ show n0 ++
                  " in " ++ (show $ Pos.sourceName $ start $ range n0) ++
                  " and " ++ (show $ Pos.sourceName $ start $ range n1))))
        (f m0) (f m1)

-- | load from disk, with import chasing
chase :: [ FilePath ] -> Identifier -> IO Program
chase dirs n =
    chaser dirs ( Program { modules = M.empty, functions = M.empty } )  n

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
                    let m = m0 { Module.source_location = ff, Module.source_text = s }
                    hPutStrLn stderr $ show m
                    foldM ( chaser dirs )
                          ( add_module m p )
                          $ map Module.source $ Module.imports m

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
