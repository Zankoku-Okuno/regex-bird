{-| A utility module to help generalize over the list-like data structures
    that can reasonably be fed into regex algorithms.

    We re-use the code from the 'ListLike' heavily.
    It already implements common functionality for data structures such as String, Text, ByteString, etc.
    One thing that's missing are pattern synonyms, so we include those.

    Finally, we've defined a typeclass synonym to constrain user-facing regex functions.
-}
module Text.Regex.Bird.Internal.List
    ( -- * ListLike module
      module Data.ListLike
    -- ** Missing Functionality
    , pattern Nil, pattern (:<|), pattern (:|>)
    , unsnoc
    -- * Regexable synonym
    , Regexable
    )
    where

import Prelude hiding (null, last, init)
import Data.ListLike
import Data.Symbol


{-| Pattern for empty 'ListLike' instances. -}
pattern Nil :: (ListLike t a) => t
pattern Nil <- (null -> True)
    where Nil = empty

{-| Pattern for non-empty 'ListLike' instances which takes the head and tail. -}
pattern (:<|) :: (ListLike t a) => a -> t -> t
pattern x :<| xs <- (uncons -> Just (x, xs))
    where x :<| xs = x `cons` xs

{-| Pattern for non-empty 'ListLike' instances which takes the init and last. -}
pattern (:|>) :: (ListLike t a) => t -> a -> t
pattern xs :|> x <- (unsnoc -> Just (xs, x))
    where xs :|> x = xs <> singleton x


{-| Same as 'uncons', but from the end of the list rather than the front.

    I'm not sure why this was missing from the 'ListLike' typeclass.
    For singly-linked lists, it would be inefficient, but not for finger-tree-based sequences.
-}
unsnoc Nil = Nothing
unsnoc xs = Just (init xs, last xs)

instance StringLike Symbol where
    fromString = intern
    toString = unintern


{-| Constraint synonym that aggregates the usual constraints on regex functions.

    We need 'Ord' constraints on both @x@ and @t@ because they are used to maintain
    environments in @Text.Regex.Bird.Internal.Env@.
    The 'ListLike'@ t a@ constraint expresses that the input is a sequence @t@ of characters in an alphabet @a@.
    The 'Eq'@ a@ constraint is required so that characters can be tested for equality, which is essential for literal patterns.
    Odds are, any 'ListLike'@ t a@ will already implement 'Eq'@ a@,
    but this property is not encoded as a superclass of 'ListLike', so we have to include it here anyway.
-}
type Regexable x t a = (Ord x, Ord t, Ord a, ListLike t a, Eq a, Enum a)
