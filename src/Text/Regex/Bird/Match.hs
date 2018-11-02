{-| This module contains funtions that match regeces agains input strings.

    The functions in this module defines return all possible matches.
    There are two main reasons why there may be multiple matches:

    - Capturing groups may be ambigious.
        For example, matching @(?x.a*)(?y.a*)@ against an input of many \'@a@\'s could
        split the input between the @x@ and @y@ capturing groups in many ways.
    - If prefixes/suffixes are allowed (e.g. 'prefixMatches'), a match might be found in multiple places
        of the same input.

    It is up to the client to decide which match meets their criteria best:
    we refuse the tempatation to guess.
-}
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


{-| This type contains all the information about how a regex matched an input string.

    It does not contain any information about the regex that made the match,
    nor does it contain any extra information about the string beyond what was
    matched. It is the client's responsibility to keep track of that info if it's relevant.
-}
data Match x t a = Match
    { wholeMatch :: t -- ^the entire input string that was matched
    , capturingGroups :: Map x t -- ^all last-seen capturing groups
    }


{-| Match the given regex against the entire input and obtain all possible matches.
    If the regex does not match the input, an empty list is returned.
    Matching with this essentially anchors the regex on both ends (i.e. @^...$@).
-}
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

{-| Match the given regex against prefixes of the input.
    If the regex does not match the input, an empty list is returned.
    This is essentially matching with a start anchor, but not an end anchor (i.e. @^...@).

    This function also returns the "continuation" for each match ---
    the tail end of the input that was unnecessary to complete the match.
    This makes it easy to repeatedly peel off prefixes from a string, if that's your use case.
-}
prefixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(Match x t a, t)]
prefixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (str, post) = (, post) <$> fullMatches r str

{-| Match the given regex with any part of the input.
    If the regex does not match the input, an empty list is returned.
    This is essentially matching without anchors.

    This function also returns the surrounding input context for each match ---
    the parts of the input before and after the matched string.
-}
infixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(t, Match x t a, t)]
infixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (pre, str) = flatTriple pre <$> prefixMatches r str
    flatTriple a (b, c) = (a, b, c)

{-| Match suffixes of the input against the given regex.
    If the regex does not match the input, an empty list is returned.
    This is essentially matching with an end anchor, but not a start anchor (i.e. @...$@).

    This function also returns the "pre-continuation" for each match ---
    the leading part of the input that was skipped.
    I'm not exactly sure if this will come in handy, but its absence would create an
    obvious gap, so I've filled it in pre-emptively.
-}
suffixMatches :: (Regexable x t a) => GRegex x t a -> t -> [(t, Match x t a)]
suffixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (pre, str) = (pre, ) <$> fullMatches r str
