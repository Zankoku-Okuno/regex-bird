{-| Convenience module for use when your problem domain focuses on lazy 'Text'.
    Adding this to your imports along with "Text.Regex.Bird" is a complete
    setup for all common regex tasks.
-}
module Text.Regex.Bird.Text.Lazy where

import Data.Symbol
import Data.Text.Lazy (Text)
import Text.Regex.Bird


{-| Specialized 'GRegex' for use with 'Text',
    because that is a very common efficiency-oriented textual type.

    Since this module is made more for speed than standard-library-ness,
    we've chosen 'Symbol' for the variable type because it has fast comparison.
-}
type Regex = GRegex Symbol Text Char
