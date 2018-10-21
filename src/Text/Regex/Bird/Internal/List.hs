module Text.Regex.Bird.Internal.List
    ( pattern Nil, pattern (:<|), pattern (:|>)
    , module Data.ListLike
    , Regexable
    )
    where

import Prelude hiding (null, last, init)
import Data.ListLike


pattern Nil :: (ListLike t a) => t
pattern Nil <- (null -> True)
    where Nil = empty

pattern (:<|) :: (ListLike t a) => a -> t -> t
pattern x :<| xs <- (uncons -> Just (x, xs))
    where x :<| xs = x `cons` xs

pattern (:|>) :: (ListLike t a) => t -> a -> t
pattern xs :|> x <- (unsnoc -> Just (xs, x))
    where xs :|> x = xs <> singleton x


unsnoc Nil = Nothing
unsnoc xs = Just (init xs, last xs)


type Regexable x t a = (Ord x, Ord t, ListLike t a, Eq a)
