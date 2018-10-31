{-| This module defines the abstract syntax of regular expressions supported by 'regex-bird'.

    The patterns defined here fall into two classes:
    core syntax, and syntactic sugar.
    The core patterns can be used as constructors and deconstructors,
    but the sugar patterns are only meant to be used as constructors.
    There are too many equivalent ways a peice of sugary pattern may be represented
    for their deconstructors to be in any way efficient, or even proven correct.
-}
module Text.Regex.Bird.Patterns
    ( GRegex

    , pattern Bot
    , pattern Str
    , pattern Seq
    , pattern Alt
    , pattern And
    , pattern Star
    , pattern Capture
    , pattern Replay

    , pattern Empty
    , pattern Char
    , pattern Option
    , pattern Plus
    ) where

import Text.Regex.Bird.Internal.List
import qualified Text.Regex.Bird.Internal.Expression as I
import Text.Regex.Bird.Internal.Expression (GRegex)


{-| A regex that never matches. -}
pattern Bot :: (Regexable x t a) => GRegex x t a
pattern Bot = I.Bot

{-| A regex that matches a specific sequence of characters. -}
pattern Str :: (Regexable x t a) => t -> GRegex x t a
pattern Str str = I.Str str

{-| A compund regex that matches one regex after another. -}
pattern Seq :: (Regexable x t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Seq r r' = I.Seq r r'

{-| A compound regex that matches if either of the given regeces match. -}
pattern Alt :: (Regexable x t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Alt r r' = I.Alt r r'

{-| A compound regex that matches when both of the given regeces match. -}
pattern And :: (Regexable x t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern And r r' = I.And r r'

{-| A regex that matches zero or more repetitions of the given regex. -}
pattern Star :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Star r = I.Star r


{-| A regex that matches only when the given regex does not match.

    Capturing groups defined by the inner regex are erased by this operator if they attempt to escape.
-}
pattern Not :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Not r = I.Not r

{-| A regex that matches exactly when the given regex does,
    but also defines a capturing group. -}
pattern Capture :: (Regexable x t a) => x -> GRegex x t a -> GRegex x t a
pattern Capture x r <- I.Capture x _ r -- TODO is it right to allow any state?
    where Capture x r = I.Capture x Nil r

{-| A regex that replays the last-seen capturing group as a literal to be matched. -}
pattern Replay :: (Regexable x t a) => x -> GRegex x t a
pattern Replay x = I.Replay x


-- Syntactic Sugar

{-| A regex that only accepts empty input. -}
pattern Empty :: (Regexable x t a) => GRegex x t a
pattern Empty = Str Nil

{-| A regex that matches only the single given character. -}
pattern Char :: (Regexable x t a) => a -> GRegex x t a
pattern Char c = Str (c :<| Nil)

{-| A regex that matches the given regex exactly, or else is skipped. -}
pattern Option :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Option r <- (const Nothing -> Just r)
    where Option r = Alt Empty r

{-| A regex that matches one or more repetitions of the given regex. -}
pattern Plus :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Plus r <- (const Nothing -> Just r)
    where Plus r = Seq r (Star r)

-- TODO bounded repetition
