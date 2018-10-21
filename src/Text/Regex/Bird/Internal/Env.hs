{-| Non-deterministic environments for regex-with-backreference.

    An environment is a finite map from variables.
    Depending on what the environment is used for, the range of the environment may vary.
    For example, when matching the range will hold matched strings,
    whereas for testing plain acceptance, the range need only hold whether the capture could accept empty.
    A non-deterministic environment is a set of these environments.

    This has been put in its own module because
    there are only a few operations that a regex-with-backreference interpreter needs to support.
    The API for '[Map var [alphabet]]' is far wider than it needs to be.

    This module is meant to be imported qualified, except for 'Env', 'NdEnv'.
-}
module Text.Regex.Bird.Internal.Env
    ( Env, NdEnv
    , empty, ok, no, one, many
    , insert, update, join
    , lookup, amb, toMap

    , test_fromLists
    ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


newtype NdEnv var t = NdEnv (Set (Env var t))
    deriving (Eq, Show)

newtype Env var t = Env (Map var t)
    deriving (Eq, Ord, Show)



empty :: Env var t
empty = Env Map.empty

ok, no :: NdEnv var t
ok = NdEnv $ Set.singleton empty
no = NdEnv Set.empty

one :: (Ord var, Ord t) => Env var t -> NdEnv var t
one = many . (:[])

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

-- FIXME documentation
-- TODO is this how I want to do it, or just return a Maybe?
lookup :: (Ord var) => Env var t -> var -> Maybe t
Env theta `lookup` x = Map.lookup x theta

{-| Iterate over possible environments. -}
amb :: NdEnv var t -> [Env var t]
amb (NdEnv envs) = Set.toAscList envs

toMap :: Env var t -> Map var t
toMap (Env env) = env


test_fromLists :: (Ord x, Ord t) => [[(x, t)]] -> NdEnv x t
test_fromLists envs = NdEnv . Set.fromList $ Env . Map.fromList <$> envs
