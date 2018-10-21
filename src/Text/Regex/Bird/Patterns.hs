module Text.Regex.Bird.Patterns
    ( GRegex

    , pattern Bot
    , pattern Str
    , pattern Seq
    , pattern Alt
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


pattern Bot :: (Regexable x t a) => GRegex x t a
pattern Bot = I.Bot

pattern Str :: (Regexable x t a) => t -> GRegex x t a
pattern Str str = I.Str str

pattern Seq :: (Regexable x t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Seq r r' = I.Seq r r'

pattern Alt :: (Regexable x t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Alt r r' = I.Alt r r'

pattern Star :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Star r = I.Star r

pattern Capture :: (Regexable x t a) => x -> GRegex x t a -> GRegex x t a
pattern Capture x r <- I.Capture x _ r -- TODO is it right to allow any state?
    where Capture x r = I.Capture x Nil r

pattern Replay :: (Regexable x t a) => x -> GRegex x t a
pattern Replay x = I.Replay x


-- Syntactic Sugar

pattern Empty :: (Regexable x t a) => GRegex x t a
pattern Empty = Str Nil

pattern Char :: (Regexable x t a) => a -> GRegex x t a
pattern Char c = Str (c :<| Nil)

pattern Option :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Option r <- (const Nothing -> Just r)
    where Option r = Alt Empty r

pattern Plus :: (Regexable x t a) => GRegex x t a -> GRegex x t a
pattern Plus r <- (const Nothing -> Just r)
    where Plus r = Seq r (Star r)

-- TODO bounded repetition
