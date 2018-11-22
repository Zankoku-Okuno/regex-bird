{-| Non-deterministic environments for regex-with-backreference.

    An environment is a finite map from variables.
    Depending on what the environment is used for, the range of the environment may vary.
    For example, when matching the range will hold matched strings,
    whereas for testing plain acceptance the range need only hold whether the capture could accept empty.
    A non-deterministic environment is a set of these environments.

    This module is a wrapper around 'Map' and 'Set' from the @containers@ package.
    I've put this in its own module because the API for @['Map' var [alphabet]]@ is far wider than it needs to be.
    There are only a few operations that a regex-with-backreference interpreter needs to support.

    This module is meant to be imported qualified, except for 'Env', 'NdEnv'.
    Its definitions are not meant to leak into user code.
-}
module Text.Regex.Bird.Internal.Env
    ( -- * Deterministic Environments
    Env
    , empty
    , insert
    , lookup
    -- * Non-deterministic Environments
    , NdEnv
    , ok
    , no
    , one
    , many
    , update
    , join
    , amb

    -- * Interfacing
    , test_fromLists
    , toMaps
    ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{-| A non-deterministic environment is really just a set of environments,
    the same way a non-deterministic finite automaton is a finite automaton with a set of states rather than a single state.

    In regular expressions, there are multiple ways to match input to a regex
    (consider the regex @(a*)*@, which could match an input full of \'@a@\'s as
    @(a)(a)...@, @(aa)(a)...@, @(aaa)(aaaaa)...@, and so on).
    Normally, this parsing ambiguity doesn't matter, since regexes only test for acceptance.
    However, when backreferences are allowed, the choice of "parse tree" influences acceptance.
    Consider @(?1=a*)(=1)a@ applied to the input @a@:
    if the capturing group consumes the first \'@a@\', then the overall match will fail because two more \'@a@\'s are expected,
    but the first capturing group could capture empty, so the backreference would expect empty,
    and there would still be the input \'@a@\' left to match with the literal,
    and the whole pattern matches.
    The simplest way to deal with this ambiguity is to carry all the options around.
-}
newtype NdEnv var t = NdEnv (Set (Env var t))
    deriving (Eq, Show)

{-| An abstract type representing environments during regex matching.

    In 'Env var t', 'var' is the type of "variables" that index the environment
    and 't' is the type of data stored at those indeces.

    For simplicity and speed, I recommend 'Data.Symbol.Symbol' for the variable type wherever possible.
    Further, 'Env's are stored into 'Set's in this package, which requires an 'Ord' instance for both 'var' and 't'.
    Storing into sets instead of lists allows us to discard duplicate possibilities in 'NdEnv'.
-}
newtype Env var t = Env (Map var t)
    deriving (Eq, Ord, Show)



{-| An environment with no capturing groups. -}
empty :: Env var t
empty = Env Map.empty

{-| An 'NdEnv' that represents acceptance without capturing groups. -}
ok :: NdEnv var t
ok = NdEnv $ Set.singleton empty

{-| An 'NdEnv' which represents a failure to accept the input. -}
no :: NdEnv var t
no = NdEnv Set.empty

{-| Lift a single (deterministic) environment into a non-deterministic environment. -}
one :: (Ord var, Ord t) => Env var t -> NdEnv var t
one = many . (:[])

{-| Create a non-deterministic environment from a list of environments. -}
many :: (Ord var, Ord t) => [Env var t] -> NdEnv var t
many = NdEnv . Set.fromList


{-| Extend an environment with one extra binding.
    If applicable, the prior value at that variable is replaced. -}
insert :: (Ord var) => Env var t -> (var, t) -> Env var t
insert (Env theta) (x, v) = Env $ Map.insert x v theta

{-| Right-biased union of two environments. -}
update :: (Ord var) => Env var t -> Env var t -> Env var t
update (Env theta) (Env theta') = Env $ theta' `Map.union` theta

{-| Union of options from two non-deterministic environments. -}
join :: (Ord var, Ord t) => NdEnv var t -> NdEnv var t -> NdEnv var t
join (NdEnv thetas) (NdEnv thetas') = NdEnv $ thetas `Set.union` thetas'


{-| Obtain a capturing group by name, if it exists. -}
lookup :: (Ord var) => Env var t -> var -> Maybe t
Env theta `lookup` x = Map.lookup x theta

{-| Iterate over possible environments. -}
amb :: NdEnv var t -> [Env var t]
amb (NdEnv envs) = Set.toAscList envs


{-| Convert list of list of pairs into a non-deterministic environment.

    Intended for testing only.
-}
test_fromLists :: (Ord x, Ord t) => [[(x, t)]] -> NdEnv x t
test_fromLists envs = NdEnv . Set.fromList $ Env . Map.fromList <$> envs

{-| Convert a non-deterministic environment into a list of maps.

    Intended only for use by other libraries, so that they don't have to import this module.
-}
toMaps :: NdEnv var t -> [Map var t]
toMaps (NdEnv envs) = unEnv <$> Set.toList envs
    where unEnv (Env env) = env
