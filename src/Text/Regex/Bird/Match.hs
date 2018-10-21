module Text.Regex.Bird.Match where

import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression
import Text.Regex.Bird.Internal.Algorithms


data Match x a = Match
    { wholeMatch :: Seq a
    , capturingGroups :: Map x (Seq a)
    }


fullMatches :: (Ord x, Ord a) => GRegex x a -> [a] -> [Match x a]
fullMatches r input = mkMatch <$> Env.amb (go r input)
    where
    go r [] = nu Env.empty r
    go r (c:str) = go (d Env.empty c r) str
    mkMatch env = Match
        { wholeMatch = Seq.fromList input
        , capturingGroups = Env.toMap env
        }

prefixMatches :: (Ord x, Ord a) => GRegex x a -> [a] -> [(Match x a, [a])]
prefixMatches r input = concatMap match splits
    where
    splits = [splitAt i input | i <- [0 .. length input]]
    match (pre, post) = (, post) <$> fullMatches r pre

-- TODO suffixMatches
-- TODO infixMatches
