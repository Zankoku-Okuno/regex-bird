{-| Convenience module for use when your problem domain focuses on 'String's.
    Adding this to your imports along with 'Text.Regex.Bird' is a complete
    setup for all common regex tasks.

    Honestly though, if you want to not see a dramatic speed penalty, use
    'Text.Regex.Bird.Text' or 'Text.Regex.Bird.Text.Lazy' instead.
-}
module Text.Regex.Bird.String where

import Text.Regex.Bird

{-| Specialized 'GRegex' for use with 'String's,
    because that is the most standard-library textual type.
-}
type Regex = GRegex String String Char
