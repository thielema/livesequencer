module Program where

import Term ( Identifier )
import Module ( Module )
import qualified Term
import qualified Module
import qualified Log
import qualified Exception
import qualified ControllerBase as Controller

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.Class ( lift )

import qualified Control.Exception as ExcBase

import qualified System.IO.Strict as StrictIO
import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import qualified System.IO.Error as Err

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( foldM, liftM4 )


data Program =
    Program
        { modules :: M.Map Module.Name Module
        , functions :: Module.FunctionDeclarations
        , constructors :: Module.ConstructorDeclarations
        , controls :: Controller.Assignments
        , controlValues :: Controller.Values
        }
--    deriving (Show)

empty :: Program
empty =
    Program {
        modules = M.empty,
        functions = M.empty,
        constructors = S.empty,
        controls = M.empty,
        controlValues = Controller.emptyValues
    }

singleton :: Module -> Program
singleton m =
    Program {
        modules = M.singleton (Module.name m) m,
        functions = Module.functions m,
        constructors = Module.constructors m,
        controls = Module.controls m,
        controlValues = Controller.emptyValues
    }

{- |
add a module

The module must not be present in the program,
otherwise this function returns an invalid 'Program'.
-}
addModule ::
    Module -> Program ->
    Exc.Exceptional Exception.Message Program
addModule m p =
    liftM4
        ( Program ( M.insert ( Module.name m ) m ( modules p ) ) )
        ( unionDecls ( Module.functions m ) ( functions p ) )
        ( fmap M.keysSet $
          unionDecls
              ( mapFromSet $ Module.constructors m )
              ( mapFromSet $ constructors p ) )
        ( Controller.union
              ( Controller.updateValues
                    ( controlValues p ) ( Module.controls m ) )
              ( controls p ) )
        ( return $ controlValues p )

removeModule ::
    Module.Name -> Program -> Program
removeModule nm p =
    case M.lookup nm $ modules p of
        Nothing -> p
        Just m -> Program {
            modules = M.delete nm $ modules p,
            functions = M.difference ( functions p ) ( Module.functions m ),
            constructors =
                S.difference ( constructors p ) ( Module.constructors m ),
            controls = M.difference ( controls p ) ( Module.controls m ),
            controlValues = controlValues p
          }

replaceModule ::
    Module -> Program ->
    Exc.Exceptional Exception.Message Program
replaceModule m p =
    addModule m $ removeModule (Module.name m) p


mapFromSet :: Ord a => S.Set a -> M.Map a ()
mapFromSet =
    M.fromAscList . map (flip (,) ()) . S.toAscList

unionDecls ::
    M.Map Identifier a ->
    M.Map Identifier a ->
    Exc.Exceptional Exception.Message ( M.Map Identifier a )
unionDecls m0 m1 =
    let f = M.mapWithKey (\nm rs -> (nm, Exc.Success rs))
    in  Trav.sequenceA . fmap snd $
        M.unionWith (\(n0,_) (n1,_) ->
            (n0,
             Exc.Exception $ Exception.Message Exception.Parse
                 (Term.range n0)
                 ("duplicate definition of " ++ show n0 ++
                  " in " ++ (Module.deconsName $ Module.nameFromIdentifier n0) ++
                  " and " ++ (Module.deconsName $ Module.nameFromIdentifier n1))))
        (f m0) (f m1)


minimize :: Module.Name -> Program -> (S.Set Module.Name, Program)
minimize seed p =
    let trace modName ms =
            if S.member modName ms
              then foldl (flip trace) (S.delete modName ms) $
                   maybe [] (map Module.source . Module.imports) $
                   M.lookup modName (modules p)
              else ms
        removed = trace seed $ M.keysSet $ modules p
    in  (removed, Fold.foldl (flip removeModule) p removed)


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
        Nothing -> do
            path <- chaseFile dirs ( Module.makeFileName n )
            load dirs ( Module.deconsName n ) path p

chaseMany ::
    [ FilePath ] -> [ Module.Name ] -> Program ->
    Exc.ExceptionalT Exception.Message IO Program
chaseMany dirs names p =
    foldM ( chaser dirs ) p names

chaseImports ::
    [ FilePath ] -> Module.Module -> Program ->
    Exc.ExceptionalT Exception.Message IO Program
chaseImports dirs =
    chaseMany dirs . map Module.source . Module.imports

load ::
    [ FilePath ] -> String -> FilePath -> Program ->
    Exc.ExceptionalT Exception.Message IO Program
load dirs n ff p = do
    content <-
        Exc.mapExceptionT
            (\e -> Exception.Message
                Exception.InOut (Exception.dummyRange ff) (Err.ioeGetErrorString e)) $
        Exc.fromEitherT $ ExcBase.try $ StrictIO.readFile ff
    m <- Exception.lift $ Module.parse n ff content
    lift $ Log.put $ show m
    chaseImports dirs m =<< Exception.lift ( addModule m p )

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
             (Exception.dummyRange f)
             (unwords [ "module", "not", "found:", f ]))
        dirs
