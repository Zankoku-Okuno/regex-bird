{-| Main import module for using 'regex-bird'.
    With this module imported, and possibly a specialization of `GRegex` to your use case,
    you'll have everything commonly needed to work with the regular expressions this library supports.
-}
module Text.Regex.Bird
    ( module Text.Regex.Bird.Patterns
    , module Text.Regex.Bird.Match
    , Regexable
    ) where

import Text.Regex.Bird.Internal.List
import Text.Regex.Bird.Patterns
import Text.Regex.Bird.Match