module Text.Regex.Bird.Match
    ( Match(..)
    , fullMatches
    , prefixMatches
    , infixMatches
    , suffixMatches
    ) where

import Prelude hiding (length, splitAt, concatMap)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Regex.Bird.Internal.List
import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression
import Text.Regex.Bird.Internal.Algorithms


data Match x t a = Match
    { wholeMatch :: t
    , capturingGroups :: Map x t
    }


fullMatches :: (Regexable x t a) => GRegex x t a -> t -> [Match x t a]
fullMatches r input = mkMatches $ go r input
    where
    go r Nil = nu Env.empty r
    go r (c :<| str) = go (d Env.empty c r) str
    mkMatches envs = mkMatch <$> Env.toMaps envs
    mkMatch env = Match
        { wholeMatch = input
        , capturingGroups = env
        }

prefixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(Match x t a, t)]
prefixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (str, post) = (, post) <$> fullMatches r str

infixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(t, Match x t a, t)]
infixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (pre, str) = flatTriple pre <$> prefixMatches r str
    flatTriple a (b, c) = (a, b, c)

suffixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(t, Match x t a)]
suffixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (pre, str) = (pre, ) <$> fullMatches r str
