module Program where

import Term ( Range (Range, start), Identifier (..) )
import Module ( Module )
import qualified Module
import qualified Log
import qualified Exception

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class ( lift )

import qualified Control.Exception as ExcBase

import Text.ParserCombinators.Parsec ( parse )
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import qualified Text.ParserCombinators.Parsec.Error as PErr

import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import qualified System.IO.Error as Err
import qualified System.FilePath as FP

import qualified Data.Traversable as Trav
import qualified Data.Map as M
import Control.Monad ( foldM )
import Data.List.HT ( chop )


data Program = Program
     { modules :: M.Map Module.Name Module
     , functions :: Module.FunctionDeclarations
     }
--    deriving (Show)

empty :: Program
empty =
    Program { modules = M.empty, functions = M.empty }

add_module ::
    Module -> Program ->
    Exc.Exceptional Exception.Message Program
add_module m p =
    fmap (\newFuncs ->
        p { modules = M.insert ( Module.name m ) m $ modules p,
            functions = newFuncs }) $
    union_functions ( Module.functions m ) $
    M.difference
        (functions p)
        (M.findWithDefault M.empty ( Module.name m )
            ( fmap Module.functions $ modules p ))

union_functions ::
    Module.FunctionDeclarations ->
    Module.FunctionDeclarations ->
    Exc.Exceptional Exception.Message Module.FunctionDeclarations
union_functions m0 m1 =
    let f = M.mapWithKey (\nm rs -> (nm, Exc.Success rs))
    in  Trav.sequenceA . fmap snd $
        M.unionWith (\(n0,_) (n1,_) ->
            (n0,
             Exc.Exception $ Exception.Message Exception.Parse
                 (range n0)
                 ("duplicate definition of " ++ show n0 ++
                  " in " ++ (show $ Pos.sourceName $ start $ range n0) ++
                  " and " ++ (show $ Pos.sourceName $ start $ range n1))))
        (f m0) (f m1)

-- | load from disk, with import chasing
chase ::
    [ FilePath ] -> Module.Name ->
    Exc.ExceptionalT Exception.Message IO Program
chase dirs n =
    chaser dirs empty n

chaser ::
    [ FilePath ] -> Program -> Module.Name ->
    Exc.ExceptionalT Exception.Message IO Program
chaser dirs p n = do
    lift $ Log.put $ "chasing " ++ Module.tellName n
    case M.lookup n ( modules p ) of
        Just _ -> lift $ do
            Log.put $ "module is already loaded"
            return p
        Nothing ->
            let nn = Module.deconsName n
            in  load dirs p nn =<<
                chaseFile dirs
                    ( FP.addExtension (FP.joinPath $ chop ('.'==) nn) "hs" )

load ::
    [ FilePath ] -> Program -> String -> FilePath ->
    Exc.ExceptionalT Exception.Message IO Program
load dirs p n ff = do
    parseResult <-
        Exc.mapExceptionT
            (\e -> Exception.Message
                Exception.InOut (dummyRange ff) (Err.ioeGetErrorString e)) $
        Exc.fromEitherT $ ExcBase.try $
        fmap (\s -> parse (Module.parseUntilEOF ff s) n s) $ readFile ff
    case parseResult of
        Left err -> Exc.throwT (messageFromParserError err)
        Right m -> do
            lift $ Log.put $ show m
            pNew <- Exc.ExceptionalT $ return $ add_module m p
            foldM ( chaser dirs ) pNew $
                map Module.source $ Module.imports m

-- | look for file, trying to append its name to the directories in the path,
-- in turn. Will fail if file is not found.
chaseFile ::
    [FilePath] -> FilePath ->
    Exc.ExceptionalT Exception.Message IO FilePath
chaseFile dirs f =
    foldr
        (\dir go -> do
            let ff = dir </> f
            e <- lift $ doesFileExist ff
            if e
              then lift $ do
                Log.put $ unwords [ "found at location", ff ]
                return ff
              else go)
        (Exc.throwT $ Exception.Message Exception.InOut
             (dummyRange f)
             (unwords [ "module", "not", "found:", f ]))
        dirs

dummyRange :: String -> Range
dummyRange f =
    let pos = Pos.initialPos f
    in  Range pos pos

messageFromParserError :: PErr.ParseError -> Exception.Message
messageFromParserError err = Exception.Message
    Exception.Parse
    (let p = PErr.errorPos err
     in  Range p (Pos.updatePosChar p ' '))
    (removeLeadingNewline $
     PErr.showErrorMessages
         "or" "unknown parse error"
         "expecting" "unexpected" "end of input" $
     PErr.errorMessages err)

removeLeadingNewline :: String -> String
removeLeadingNewline ('\n':str) = str
removeLeadingNewline str = str
