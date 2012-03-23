{-
This is a part of the Controls module
that is separated in order to prevent an import cycle.
-}
module ControlsBase where

import qualified Exception
import qualified Term
import Term ( Term )

import qualified Data.Map as M

import qualified Control.Monad.Exception.Synchronous as Exc

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import qualified Data.Traversable as Trav



data Control = CheckBox Bool

newtype Name = Name String
    deriving (Eq, Ord, Show)

deconsName :: Name -> String
deconsName (Name name) = name


type Assignments = M.Map Name (Term.Range, Control)


exc :: Term.Range -> String -> Exception.Message
exc rng msg =
    Exception.Message Exception.Parse rng msg

excDuplicate :: Name -> Term.Range -> Exception.Message
excDuplicate name rng =
    exc rng $
        "duplicate controller definition with name "
         ++ deconsName name

union ::
    Assignments ->
    Assignments ->
    Exc.Exceptional Exception.Message Assignments
union m0 m1 =
    let f = fmap Exc.Success
    in  Trav.sequenceA $
        M.unionWithKey
            (\name _ a -> do
                (rng, _c) <- a
                Exc.throw $ excDuplicate name rng)
            (f m0) (f m1)

collect ::
    Term -> Exc.Exceptional Exception.Message Assignments
collect topTerm =
    flip MS.execStateT M.empty $
    mapM_
        (\ea -> do
            (name, rc@(rng, _ctrl)) <- MT.lift ea
            MT.lift . Exc.assert (excDuplicate name rng)
                =<< MS.gets (not . M.member name)
            MS.modify (M.insert name rc)) $ do

    ( _pos, term ) <- Term.subterms topTerm
    case Term.viewNode term of
        Just ( "checkBox" , args ) ->
            return $
            case args of
                [ Term.StringLiteral _rng tag, Term.Node val [] ] ->
                    case reads $ Term.name val of
                        [(b, "")] ->
                            Exc.Success $ (Name tag, (Term.termRange term, CheckBox b))
                        _ ->
                            Exc.Exception $
                            exc (Term.range val) $
                            "cannot parse Bool value " ++
                            show (Term.name val) ++ " for getBox"
                _ ->
                    Exc.Exception $
                    exc (Term.termRange term) "invalid checkBox arguments"
        _ -> []
