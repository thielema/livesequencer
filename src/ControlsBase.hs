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



data Control =
      CheckBox Bool
    | Slider Int Int Int

data Value = Bool Bool | Number Int
    deriving Show

data Values =
    Values {
        boolValues :: M.Map Name Bool,
        numberValues :: M.Map Name Int
     } deriving Show

newtype Name = Name String
    deriving (Eq, Ord, Show)

deconsName :: Name -> String
deconsName (Name name) = name


emptyValues :: Values
emptyValues = Values M.empty M.empty

updateValues :: Values -> Assignments -> Assignments
updateValues (Values bools numbers) assigns =
    M.union
        (M.intersectionWith
            (\b (rng, a) -> (rng,
                case a of
                    CheckBox _deflt -> CheckBox b
                    _ -> a))
            bools assigns) $
    M.union
        (M.intersectionWith
            (\x (rng, a) -> (rng,
                case a of
                    Slider lower upper _deflt -> Slider lower upper x
                    _ -> a))
            numbers assigns) $
    assigns


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
                [ Term.StringLiteral _rng tag, Term.Node deflt [] ] ->
                    case reads $ Term.name deflt of
                        [(b, "")] ->
                            Exc.Success $ (Name tag, (Term.termRange term, CheckBox b))
                        _ ->
                            Exc.Exception $
                            exc (Term.range deflt) $
                            "cannot parse Bool value " ++
                            show (Term.name deflt) ++ " for checkBox"
                _ ->
                    Exc.Exception $
                    exc (Term.termRange term) "invalid checkBox arguments"
        Just ( "slider" , args ) ->
            return $
            case args of
                [ Term.StringLiteral _rngT tag, lower, upper, deflt ] -> do
                    let milliard = 1000000000
                        number arg =
                            Exception.checkRange
                                Exception.Parse arg id id
                                (-milliard) milliard
                    l <- number "lower slider bound" lower
                    u <- number "upper slider bound" upper
                    x <- number "default slider value" deflt
                    return (Name tag, (Term.termRange term, Slider l u x))
                _ ->
                    Exc.Exception $
                    exc (Term.termRange term) "invalid slider arguments"
        _ -> []
